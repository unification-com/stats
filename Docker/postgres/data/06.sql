DROP TABLE IF EXISTS stats.accounts;

CREATE TABLE stats.accounts (
    id SERIAL NOT NULL,
    account TEXT NOT NULL,
    mnemonic TEXT NOT NULL,
    PRIMARY KEY (id),
    UNIQUE (account, mnemonic)
)

;

