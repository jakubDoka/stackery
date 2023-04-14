use bcrypt::BcryptResult;
use uuid::Uuid;

/// Creates a set of user secret and its hashed value.
pub fn create_set(username: &str) -> BcryptResult<(Uuid, String)> {
    let secret = Uuid::new_v4();

    let salt = load_salt(username);

    let parts = bcrypt::hash_with_salt(secret.as_bytes(), bcrypt::DEFAULT_COST, salt)?;

    Ok((secret, parts.to_string()))
}

fn load_salt(username: &str) -> [u8; 16] {
    let mut salt = [0; 16];
    let len = username.len().min(16);
    salt[..len].copy_from_slice(&username.as_bytes()[..len]);
    salt
}

pub fn verify_set(secret: Uuid, hash: &str) -> BcryptResult<bool> {
    bcrypt::verify(secret.as_bytes(), hash)
}
