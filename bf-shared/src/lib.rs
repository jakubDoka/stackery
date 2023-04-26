pub mod db {
    pub const NAME: &str = "db";
    pub const PRIMARY_KEY: &str = "_id";

    pub mod user {
        pub const COLLECTION: &str = "user";
        pub const NAME: &str = super::PRIMARY_KEY;
        pub const PASSWORD_HASH: &str = "password_hash";
    }

    pub mod session {
        use std::time::Duration;

        pub const COLLECTION: &str = "session";
        pub const ID: &str = super::PRIMARY_KEY;
        pub const DURATION: Duration = Duration::from_secs(60 * 60);
    }

    pub mod post {
        pub const COLLECTION: &str = "post";
        pub const NAME: &str = super::PRIMARY_KEY;
        pub const AUTHOR: &str = "author";
        pub const CODE: &str = "code";
        pub const MAX_CODE_LENGTH: usize = 1024 * 8;

        pub fn validate_code(code: &str) -> Result<(), CodeError> {
            if code.len() > MAX_CODE_LENGTH {
                return Err(CodeError::TooLong);
            }

            Ok(())
        }

        pub enum CodeError {
            TooLong,
        }

        impl std::fmt::Display for CodeError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    Self::TooLong => write!(f, "code is too long"),
                }
            }
        }
    }

    pub const MAX_NAME_LENGTH: usize = 64;

    pub fn validate_name(name: &str) -> Result<(), NameError> {
        if name.len() > MAX_NAME_LENGTH {
            return Err(NameError::TooLong(name.len()));
        }

        Ok(())
    }

    pub enum NameError {
        TooLong(usize),
    }

    impl std::fmt::Display for NameError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::TooLong(got) => write!(
                    f,
                    "name is too long, limit is {}, got {}",
                    MAX_NAME_LENGTH, got
                ),
            }
        }
    }
}

pub mod api {
    pub mod user {
        #[derive(serde::Deserialize, serde::Serialize)]
        #[cfg_attr(feature = "open-api", derive(poem_openapi::Object))]
        pub struct RegisterForm {
            pub name: String,
            pub password_hash: String,
        }

        pub fn load_password_salt(username: &str) -> [u8; 16] {
            let mut salt = [0; 16];
            let len = username.len().min(16);
            salt[..len].copy_from_slice(&username.as_bytes()[..len]);
            salt
        }

        #[derive(serde::Deserialize, serde::Serialize)]
        #[cfg_attr(feature = "open-api", derive(poem_openapi::Object))]
        pub struct LoginForm {
            pub name: String,
            pub password_hash: String,
        }
    }

    pub mod post {
        #[derive(serde::Deserialize, serde::Serialize)]
        #[cfg_attr(
            feature = "open-api",
            derive(poem_openapi::Object),
            oai(rename = "ApiPost")
        )]
        pub struct Model {
            pub name: String,
            pub code: String,
        }
    }

    pub mod session {
        pub const KEY: &'static str = "session";
    }
}

pub mod search {
    pub mod user {
        pub const INDEX: &str = "user";
        pub const PRIMARY_KEY: &str = "name";
        pub const RESULT_LIMIT: usize = 100;
    }

    pub mod post {
        pub const INDEX: &str = "post";
        pub const PRIMARY_KEY: &str = "name";
        pub const RESULT_LIMIT: usize = 20;

        #[derive(serde::Serialize, serde::Deserialize, PartialEq, Eq, Debug)]
        #[cfg_attr(
            feature = "open-api",
            derive(poem_openapi::Object),
            oai(rename = "SearchPost")
        )]
        pub struct Model {
            pub name: String,
            pub author: String,
            pub code: String,
        }
    }
}
