#[macro_export]
macro_rules! config_var {
    ($(
        $vis:vis $name:ident: $ty:ty $(= $default:expr)?;
    )*) => {$(
        $vis static $name: std::sync::LazyLock<$ty> = std::sync::LazyLock::new(|| {
            $crate::config::FromVar::from_var(stringify!($name)).unwrap_or_else(|e| {
                if let Some(default) = $crate::config_var!(@default $($default)?) {
                    default
                } else {
                    panic!("failed to obtain '{}': {e}", stringify!($name));
                }
            })
        });
    )*};

    (@default $default:expr) => {
        if cfg!(debug_assertions) {
            Some($default)
        } else {
            None
        }
    };

    (@default) => {
        None
    };
}

pub trait FromVar: Sized {
    fn parse(var: String) -> Result<Self, String>;

    fn from_var(var: &str) -> Result<Self, String> {
        let var = std::env::var(var).map_err(|e| format!("{var} not set: {e}"))?;
        Self::parse(var)
    }
}

impl FromVar for String {
    fn parse(var: String) -> Result<Self, String> {
        Ok(var)
    }
}

impl<T: FromVar> FromVar for Option<T> {
    fn parse(var: String) -> Result<Self, String> {
        Ok(Some(T::parse(var)?))
    }

    fn from_var(var: &str) -> Result<Self, String> {
        match std::env::var(var) {
            Ok(var) => Self::parse(var),
            Err(std::env::VarError::NotPresent) => Ok(None),
            Err(std::env::VarError::NotUnicode(..)) => Err(format!("{var} is not unicode encoded")),
        }
    }
}

macro_rules! derive_from_var {
    ($($ty:ty),*) => {
        $(
            impl FromVar for $ty {
                fn parse(var: String) -> Result<Self, String> {
                    var.parse().map_err(|e| format!("failed to parse {var}: {e}"))
                }
            }
        )*
    };
}

derive_from_var!(usize, u16);
