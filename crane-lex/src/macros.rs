#[macro_export]
macro_rules! symbols {
	{$($name:ident = $v:literal),+$(,)?} => {
		pub enum Symbol {
			$(
				$name
			),*
		}

		impl TryFrom<&str> for Symbol {
			type Error = ();

			fn try_from(value: &str) -> Result<Self, Self::Error> {
				match value {
					$($v => Ok(Self::$name),)+
					_ => Err(())
				}
			}
		}

		impl TryFrom<String> for Symbol {
			type Error = ();

			fn try_from(value: String) -> Result<Self, Self::Error> {
				match &*value {
					$($v => Ok(Self:: $name),)+
					_ => Err(())
				}
			}
		}
    };
}
