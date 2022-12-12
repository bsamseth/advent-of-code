use rusqlite::{Connection, Result};

pub struct Cache {
    connection: Connection,
}

impl Cache {
    pub fn new() -> Self {
        let directory = shellexpand::tilde("~/.cache/aocd").to_string();
        std::fs::create_dir_all(&directory)
            .expect(&format!("Faled to create cache directory: {}", directory));

        let connection = Connection::open(&format!("{}/aocd.sqlite", directory))
            .expect("Failed to open cache database");

        connection
            .execute(
                "CREATE TABLE IF NOT EXISTS puzzle_input (
                   year         INTEGER NOT NULL,
                   day          INTEGER NOT NULL,
                   input        TEXT NOT NULL,
                   PRIMARY KEY  (year, day)
                 )",
                [],
            )
            .expect("Failed to create puzzle_input cache table");
        connection
            .execute(
                "CREATE TABLE IF NOT EXISTS puzzle_answer (
                   year         INTEGER NOT NULL,
                   day          INTEGER NOT NULL,
                   part         INTEGER NOT NULL,
                   answer       TEXT NOT NULL,
                   correct      BOOLEAN NOT NULL,
                   response     TEXT NOT NULL,
                   PRIMARY KEY  (year, day, part, answer)
                  )",
                [],
            )
            .expect("Failed to create puzzle_answer cache table");

        Self { connection }
    }

    pub fn cache_answer_response(
        &self,
        year: u16,
        day: u8,
        part: u8,
        answer: impl ToString,
        response: &str,
        correct: bool,
    ) {
        self.connection
            .execute(
                "INSERT OR REPLACE INTO puzzle_answer (year, day, part, answer, correct, response)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                (year, day, part, answer.to_string(), correct, response),
            )
            .expect("Failed to cache puzzle answer response");
    }

    pub fn get_correct_answer(&self, year: u16, day: u8, part: u8) -> Result<String> {
        let mut statement = self
            .connection
            .prepare("SELECT answer FROM puzzle_answer WHERE year = ?1 AND day = ?2 AND part = ?3 AND correct")
            .expect("Failed to prepare get correct answer statement");
        let mut rows = statement.query((year, day, part))?;
        match rows.next()? {
            Some(row) => Ok(row.get(0)?),
            None => Err(rusqlite::Error::QueryReturnedNoRows),
        }
    }

    pub fn get_answer_response(
        &self,
        year: u16,
        day: u8,
        part: u8,
        answer: &str,
    ) -> Result<String> {
        let mut statement = self
            .connection
            .prepare(
                "SELECT response
                 FROM puzzle_answer
                 WHERE year = ?1 AND day = ?2 AND part = ?3 AND answer = ?4",
            )
            .expect("Failed to prepare get_answer_response query");
        let mut rows = statement.query((year, day, part, answer.to_string()))?;
        match rows.next()? {
            Some(cached) => Ok(cached.get(0)?),
            _ => Err(rusqlite::Error::QueryReturnedNoRows),
        }
    }

    pub fn get_input(&self, year: u16, day: u8) -> Result<String> {
        let mut statement = self
            .connection
            .prepare("SELECT input FROM puzzle_input WHERE year = ? AND day = ?")
            .expect("Failed to prepare puzzle_input query");
        let row = statement.query_map((year, day), |row| row.get(0))?.next();
        match row {
            Some(input) => input,
            None => Err(rusqlite::Error::QueryReturnedNoRows),
        }
    }

    pub fn cache_input(&self, year: u16, day: u8, input: &str) {
        self.connection
            .execute(
                "INSERT OR REPLACE INTO puzzle_input (year, day, input) VALUES (?1, ?2, ?3)",
                (year, day, input),
            )
            .expect("Failed to insert puzzle_input into cache");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache() {
        let cache = Cache::new();
    }
}
