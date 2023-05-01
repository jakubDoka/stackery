use std::{collections::VecDeque, io, sync::Arc, time::Duration};

use bf_shared::db;
use dashmap::DashSet;
use tokio::time::Instant;
use uuid::Uuid;

#[derive(Clone)]
pub struct Sessions {
    sessions: Arc<DashSet<Uuid>>,
    session_timeouts: tokio::sync::mpsc::Sender<Uuid>,
}

impl Sessions {
    pub fn new() -> Self {
        let map = Arc::new(DashSet::new());
        let (tx, rx) = tokio::sync::mpsc::channel(100);
        tokio::spawn(
            ExpireWorker {
                sessions: map.clone(),
                session_timeouts: rx,
                queue: VecDeque::new(),
            }
            .run(),
        );
        Self {
            sessions: map,
            session_timeouts: tx,
        }
    }

    pub async fn create(&self) -> Uuid {
        let session_id = Uuid::new_v4();
        self.sessions.insert(session_id);
        self.session_timeouts
            .send(session_id)
            .await
            .expect("session timeout channel closed");
        session_id
    }

    pub(crate) fn is_active(&self, salt: Uuid) -> bool {
        self.sessions.contains(&salt)
    }

    pub fn remove(&self, salt: Uuid) {
        self.sessions.remove(&salt);
    }
}

pub struct AppStateManager;

impl AppStateManager {
    pub async fn load_app_state(user_name: &str) -> Option<String> {
        let file_name = Self::file_name(user_name);

        match tokio::fs::read_to_string(&file_name).await {
            Ok(json) => Some(json),
            Err(err) if err.kind() == io::ErrorKind::NotFound => None,
            Err(err) => {
                crate::log!(error, "failed to read app state file"; "file" => file_name, "error" => %err);
                None
            }
        }
    }

    pub async fn store_app_state(user_name: &str, app_state: String) {
        let file_name = Self::file_name(user_name);

        if let Err(err) = tokio::fs::write(&file_name, app_state).await {
            crate::log!(error, "failed to write app state file"; "file" => file_name, "error" => %err);
        }
    }

    fn file_name(user_name: &str) -> String {
        format!(
            "{}/{}.json",
            db::session::STATE_DIR,
            Self::encode_name_base16(user_name)
        )
    }

    fn encode_name_base16(name: &str) -> String {
        let mut result = String::with_capacity(name.len() * 2);
        for byte in name.bytes() {
            const HEX_CHARS: [u8; 16] = *b"0123456789abcdef";
            result.push(HEX_CHARS[(byte >> 4) as usize] as char);
            result.push(HEX_CHARS[(byte & 0xf) as usize] as char);
        }
        result
    }
}

struct ExpireWorker {
    sessions: Arc<DashSet<Uuid>>,
    session_timeouts: tokio::sync::mpsc::Receiver<Uuid>,
    queue: VecDeque<ExpireTask>,
}

impl ExpireWorker {
    async fn run(mut self) {
        loop {
            tokio::select! {
                _ = tokio::time::sleep_until(self.queue.front().map_or(
                    Instant::now() + Duration::from_secs(100),
                    |t| t.created_at + bf_shared::db::session::DURATION)),
                if !self.queue.is_empty() => {
                    let task = self.queue.pop_front().unwrap();
                    self.sessions.remove(&task.session_id);
                }
                result = self.session_timeouts.recv() => {
                    let Some(session_id) = result else { break };
                    self.queue.push_back(ExpireTask {
                        created_at: Instant::now(),
                        session_id,
                    });
                }
            }
        }
    }
}

struct ExpireTask {
    created_at: Instant,
    session_id: Uuid,
}
