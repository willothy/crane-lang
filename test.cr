type Test = root::test;

pub type External = ::package::Type;

pub type Alias = super::super::test;

struct Testing {
	a: self::Alias,
	b: test
}

static S: i32 = 5;


pub fn inc(x: f32) -> f32 { x + 1 }
pub fn dec(x: f32) -> f32 { x - 1 }

const C: u8 = 2;


pub fn func() -> f32 {
	loop {
		let a: f32 = test(5 + 5);
		let x: fn(i32) -> i32 = fn(x: i32) -> i32 => 0;
	};

	let t: Testing = Testing {
		a: 5,
		b: 10
	};

	let t2: Testing = Testing {
		a: 5,
		b: 10,
	};

	let Testing {
		a: a,
		b: b
	} = t;

	let x: i32;

	let y = 5;
	let z = &y;
	*z = 1;

	let (a, b): (i32, i32) = (1, 2);

	let messages: [str; 2] = ["hello", "goodbye"];
	let hello: _ = fn() {
		io::stdout().write(messages[0]);
	};

	let x: _ = c as *i32;

	continue;

	break;

	break 5;

	let x: _ = ::Test::test(5);

	while true {
	 	let a: f32 = test(5 + 5);
	};

	if 2 == 5 {
		let a: *f32 = &test(5 + 5);
		a
	};

	let a: i32 = if x == 0 { 
		5
	} else { 
		6
	};

	let a: f32 = test(5 + 5);
	
	a + a
}

