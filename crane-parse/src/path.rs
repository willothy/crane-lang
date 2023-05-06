use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum ItemPath {
    /// Starting with root::
    Absolute(Vec<String>),
    /// Starting with self:: or nothing
    Relative(Vec<String>),
    /// Starting with :: followed by an external package name
    External(Vec<String>),
    /// Single identifier (name expected to be in scope)
    Name(String),
}

impl Display for ItemPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemPath::Absolute(path) => {
                for (i, part) in path.iter().enumerate() {
                    if i > 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", part)?;
                }
            }
            ItemPath::Relative(path) => {
                for (i, part) in path.iter().enumerate() {
                    if i > 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", part)?;
                }
            }
            ItemPath::External(path) => {
                write!(f, "::")?;
                for (i, part) in path.iter().enumerate() {
                    if i > 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", part)?;
                }
            }
            ItemPath::Name(name) => write!(f, "{}", name)?,
        }
        Ok(())
    }
}

/// Item path starting with root:: or :: (external)
/// Guaranteed to be absolute
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum AbsoluteItemPath {
    Absolute(Vec<String>),
    External(Vec<String>),
}

impl TryInto<AbsoluteItemPath> for ItemPath {
    type Error = ();

    fn try_into(self) -> Result<AbsoluteItemPath, Self::Error> {
        match self {
            ItemPath::Absolute(path) => Ok(AbsoluteItemPath::Absolute(path)),
            ItemPath::External(path) => Ok(AbsoluteItemPath::External(path)),
            _ => Err(()),
        }
    }
}

impl ItemPath {
    pub fn name(&self) -> Option<&String> {
        match self {
            ItemPath::Absolute(path) => path.last(),
            ItemPath::Relative(path) => path.last(),
            ItemPath::External(path) => path.last(),
            ItemPath::Name(name) => Some(name),
        }
    }

    pub fn into_absolute(self) -> Option<AbsoluteItemPath> {
        match self {
            ItemPath::Absolute(path) => Some(AbsoluteItemPath::Absolute(path)),
            ItemPath::External(path) => Some(AbsoluteItemPath::External(path)),
            _ => None,
        }
    }

    pub fn root(&self) -> Option<&String> {
        match self {
            ItemPath::Absolute(path) => path.first(),
            ItemPath::Relative(path) => path.first(),
            ItemPath::External(path) => path.first(),
            ItemPath::Name(_) => None,
        }
    }

    pub fn is_absolute(&self) -> bool {
        matches!(self, ItemPath::Absolute(_))
    }

    pub fn is_relative(&self) -> bool {
        matches!(self, ItemPath::Relative(_))
    }

    pub fn is_external(&self) -> bool {
        matches!(self, ItemPath::External(_))
    }

    pub fn is_name(&self) -> bool {
        matches!(self, ItemPath::Name(_))
    }
}
