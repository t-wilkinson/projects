-- Up
CREATE TABLE IF NOT EXISTS emails (
    email TEXT NOT NULL PRIMARY KEY,
    UNIQUE(email)
);

CREATE TABLE IF NOT EXISTS tokens (
    token TEXT NOT NULL,
    email TEXT NOT NULL,
    FOREIGN KEY(email) REFERENCES emails(email)
);

-- Down
DROP TABLE emails;
DROP TABLE tokens;
