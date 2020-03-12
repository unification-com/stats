DROP TABLE IF EXISTS stats.validators;

CREATE TABLE stats.validators (
    id SERIAL NOT NULL,
    operator_address TEXT NOT NULL,
    tokens BIGINT NOT NULL,
    delegator_shares DOUBLE PRECISION NOT NULL,
    rate DOUBLE PRECISION NOT NULL,
    jailed BOOLEAN NOT NULL,
    utc_date TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    PRIMARY KEY (id)
)

;
