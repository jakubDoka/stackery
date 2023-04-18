pub struct Posts {
    db: mongodb::Database,
}

impl Posts {
    const TABLE: &'static str = "post";
    const NAME: &'static str = "_id";
    const AUTHOR: &'static str = "author";
    const CONTENT: &'static str = "content";

    pub fn new(db: mongodb::Database) -> Self {
        Self { db }
    }
}
