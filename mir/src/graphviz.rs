use crane_parse::{package::pass::Inspect, unit::Unit};

use crate::{
    builder::MIRBuilder,
    instruction::{MIRNode, MIRTermination},
    Item, ItemId, MIRPackage, UnitId,
};

pub struct FnGraphVizPass<'a> {
    result: String,
    marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> FnGraphVizPass<'a> {
    pub fn new() -> Self {
        Self {
            result: String::new(),
            marker: std::marker::PhantomData,
        }
    }

    pub fn result(&self) -> &str {
        &self.result
    }
}

impl<'a> Inspect for FnGraphVizPass<'a> {
    type Scope = MIRPackage;

    type Input = (UnitId, ItemId, &'a MIRBuilder);

    fn inspect(&mut self, scope: &Self::Scope, input: Self::Input) {
        let unit = scope.unit(input.0).unwrap();
        let func = unit.items.get(input.1).unwrap();
        let Item::Function { name: fn_name, ty: _, body } = func else {
            panic!()
        };
        let mut nodes = vec![];
        let mut edges = vec![];
        for (i, (block_name, block_id)) in body.iter().enumerate() {
            let node = format!("N{i}[label=\"{}\", shape=\"box\"];", block_name);
            nodes.push(node);
            match unit.blocks.get(*block_id).unwrap().termination {
                Some(term) => match unit.node(term).unwrap() {
                    MIRNode::Termination { inst } => match inst {
                        MIRTermination::Return { value: _ } => {}
                        MIRTermination::Branch { cond, then, r#else } => {
                            let then_name = &unit.blocks.get(*then).unwrap().name;
                            let then = body.iter().position(|v| v.0 == then_name).unwrap();

                            let else_name = &unit.blocks.get(*r#else).unwrap().name;
                            let r#else = body.iter().position(|v| v.0 == else_name).unwrap();
                            edges.push(format!("N{} -> N{}[label=\"{:?}\"]", i, then, cond));
                            edges.push(format!("N{} -> N{}[label=\"!{:?}\"]", i, r#else, cond));
                        }
                        MIRTermination::Jump { target } => {
                            let target_name = &unit.blocks.get(*target).unwrap().name;
                            let target = body.iter().position(|v| v.0 == target_name).unwrap();
                            edges.push(format!("N{} -> N{}", i, target));
                        }
                        MIRTermination::JumpIf { cond, then } => {
                            let then_name = &unit.blocks.get(*then).unwrap().name;
                            let then = body.iter().position(|v| v.0 == then_name).unwrap();
                            edges.push(format!("N{} -> N{}[label=\"{:?}\"]", i, then, cond));
                        }
                    },
                    _ => panic!("Expected termination"),
                },
                None => {}
            }
            let mut lines = vec![];
            lines.push(format!("digraph {} {{", fn_name));
            for node in nodes.iter() {
                lines.push(format!("    {}", node));
            }
            for edge in edges.iter() {
                lines.push(format!("    {}", edge));
            }
            lines.push("}".to_string());
            self.result = lines.join("\n");
        }
    }
}
