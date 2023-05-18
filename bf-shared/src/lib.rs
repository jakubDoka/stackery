pub mod db {
    pub const NAME: &str = "db";
    pub const PRIMARY_KEY: &str = "_id";

    pub mod user {
        pub const COLLECTION: &str = "user";
        pub const NAME: &str = super::PRIMARY_KEY;
        pub const PASSWORD_HASH: &str = "password_hash";
    }

    pub mod session {
        use std::{fmt, time::Duration};

        pub const COLLECTION: &str = "session";
        pub const ID: &str = super::PRIMARY_KEY;
        pub const DURATION: Duration = Duration::from_secs(60 * 60);
        pub const STATE_DIR: &str = "app-state";

        #[derive(serde::Serialize, serde::Deserialize, PartialEq, Eq)]
        #[cfg_attr(feature = "open-api", derive(poem_openapi::Object))]
        pub struct AppState {
            pub user_name: String,
            pub theme: Theme,
            pub routers: String,
            pub drafts: Vec<PostDraft>,
        }

        impl AppState {
            pub const DRAFT_MAX_SIZE: usize = 1024 * 1024 * 10;

            pub fn new(user_name: &str) -> Self {
                Self {
                    user_name: user_name.to_owned(),
                    theme: Theme::Dark,
                    routers: String::new(),
                    drafts: Vec::new(),
                }
            }

            pub fn has_full_drafts(&self) -> bool {
                self.drafts.iter().map(|d| d.code.len()).sum::<usize>() >= Self::DRAFT_MAX_SIZE
            }
        }

        #[derive(serde::Serialize, serde::Deserialize, Clone, Copy, PartialEq, Eq)]
        #[cfg_attr(feature = "open-api", derive(poem_openapi::Enum))]
        pub enum Theme {
            Light,
            Dark,
        }

        impl fmt::Display for Theme {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    Self::Light => write!(f, "light"),
                    Self::Dark => write!(f, "dark"),
                }
            }
        }

        #[derive(serde::Serialize, serde::Deserialize, PartialEq, Eq)]
        #[cfg_attr(feature = "open-api", derive(poem_openapi::Object))]
        pub struct PostDraft {
            pub name: String,
            pub code: String,
        }
    }

    pub mod post {
        pub const COLLECTION: &str = "post";
        pub const NAME: &str = super::PRIMARY_KEY;
        pub const AUTHOR: &str = "author";
        pub const CODE: &str = "code";
        pub const MAX_CODE_LENGTH: usize = 1024 * 8;

        #[derive(serde::Serialize, serde::Deserialize)]
        pub struct Model {
            #[serde(rename = "_id")]
            pub name: String,
            pub author: String,
            pub code: String,
        }

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

    pub const ALLOWED_NAME_CHARS: &[(u8, u8)] = &[
        (b'a', b'z'),
        (b'A', b'Z'),
        (b'0', b'9'),
        (b'_', b'_'),
        (b'-', b'-'),
    ];
    pub const MAX_NAME_LENGTH: usize = 64;

    pub fn validate_name(name: &str) -> Result<(), NameError> {
        if name.len() > MAX_NAME_LENGTH {
            return Err(NameError::TooLong(name.len()));
        }

        const fn indexes(b: u8) -> (usize, usize) {
            (b as usize >> 6, b as usize & ((1 << 6) - 1))
        }

        const VALID_BYTES: [u64; 4] = {
            let mut bitset = [0; 4];

            let mut i = 0;
            while i < ALLOWED_NAME_CHARS.len() {
                let (start, end) = ALLOWED_NAME_CHARS[i];

                let mut j = start;
                while j <= end {
                    let (byte, bit) = indexes(j);
                    bitset[byte] |= 1 << bit;
                    j += 1;
                }

                i += 1;
            }

            bitset
        };

        let valid_chars = name.as_bytes().iter().all(|&b| {
            let (byte, bit) = indexes(b);
            VALID_BYTES[byte] & 1 << bit != 0
        });

        if !valid_chars {
            return Err(NameError::InvalidChars);
        }

        Ok(())
    }

    pub enum NameError {
        TooLong(usize),
        InvalidChars,
    }

    impl std::fmt::Display for NameError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::TooLong(got) => write!(
                    f,
                    "name is too long, limit is {}, got {}",
                    MAX_NAME_LENGTH, got
                ),
                Self::InvalidChars => {
                    write!(
                        f,
                        "name contains disallowed characters, allowed characters: ",
                    )?;

                    for (i, &(start, end)) in ALLOWED_NAME_CHARS.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }

                        match start == end {
                            true => write!(f, "'{}'", start as char)?,
                            false => write!(f, "'{}'-'{}'", start as char, end as char)?,
                        }
                    }

                    Ok(())
                }
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

        impl From<crate::db::post::Model> for Model {
            fn from(post: crate::db::post::Model) -> Self {
                Self {
                    name: post.name,
                    author: post.author,
                    code: post.code,
                }
            }
        }
    }

    #[derive(Clone, Copy)]
    pub struct Query<'a> {
        pub reminder: &'a str,
        pub expects_field: bool,
    }

    impl<'a> Query<'a> {
        pub fn new(query: &'a str) -> Self {
            Self {
                reminder: query,
                expects_field: true,
            }
        }

        pub fn get_field(mut self, name: &str) -> Option<&'a str> {
            self.find_map(|(n, v)| if n == name { Some(v) } else { None })
        }

        pub fn reminder(&self) -> &'a str {
            self.reminder
        }

        pub fn set_field(mut self, filed_name: &str, new_value: &str) -> String {
            let mut query = String::new();
            let mut set = false;
            for (name, value) in self.by_ref() {
                query.push('\\');
                query.push_str(name);
                query.push(' ');

                if name == filed_name {
                    query.push_str(new_value);
                    set = true;
                } else {
                    query.push_str(value);
                }

                query.push(' ');
            }

            if !set {
                query.push('\\');
                query.push_str(filed_name);
                query.push(' ');
                query.push_str(new_value);
                query.push(' ');
            }

            query.push_str(self.reminder);

            query
        }
    }

    impl<'a> Iterator for Query<'a> {
        type Item = (&'a str, &'a str);

        fn next(&mut self) -> Option<Self::Item> {
            if !self.expects_field {
                return None;
            }

            let Some(next) = self.reminder.strip_prefix('\\') else {
                self.expects_field = false;
                return None;
            };

            fn is_field_char(c: char) -> bool {
                c.is_ascii_alphanumeric() || c == '_'
            }

            let end = next
                .find(|c| !is_field_char(c))
                .unwrap_or_else(|| next.len());

            let (field, reminder) = next.split_at(end);
            let reminder = reminder.trim_start();

            if !reminder.starts_with('"') {
                let (query, reminder) = reminder.split_once(' ').unwrap_or((reminder, ""));
                self.reminder = reminder;
                return Some((field, query));
            }

            let end = reminder
                .char_indices()
                .skip(1) // skip leading '"'
                .scan(false, |in_escape, (i, c)| {
                    let res = !*in_escape && c == '"';
                    *in_escape = !*in_escape && c == '\\';
                    Some((i + 1, res))
                })
                .find_map(|(i, end)| end.then_some(i))
                .unwrap_or(reminder.len());

            let (query, reminder) = reminder.split_at(end);

            self.reminder = reminder.trim_start();

            Some((field, query))
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_query() {
            let mut query = Query::new("\\name hello \\author \"hello world\" reminder");

            assert_eq!(query.next(), Some(("name", "hello")));
            assert_eq!(query.next(), Some(("author", "\"hello world\"")));
            assert_eq!(query.next(), None);

            assert_eq!(query.reminder(), "reminder");
        }

        #[test]
        fn test_query_reconstruct() {
            let query = Query::new("\\name hello \\author \"hello world\" reminder");

            assert_eq!(
                query.set_field("name", "world"),
                "\\name world \\author \"hello world\" reminder"
            );

            assert_eq!(
                query.set_field("author", "world"),
                "\\name hello \\author world reminder"
            );

            assert_eq!(
                query.set_field("code", "world"),
                "\\name hello \\author \"hello world\" \\code world reminder"
            );
        }
    }
}
