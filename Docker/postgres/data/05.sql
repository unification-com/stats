ALTER TABLE stats.metrics ADD machine TEXT DEFAULT 'general';
ALTER TABLE stats.metricsf ADD machine TEXT DEFAULT 'general';

DROP TABLE IF EXISTS stats.strings;

CREATE TABLE stats.strings (
    id SERIAL NOT NULL,
    metric TEXT NOT NULL,
    feature TEXT NOT NULL,
    sample TEXT NOT NULL,
    machine TEXT DEFAULT 'general',
    utc_date TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    PRIMARY KEY (id)
)

;

CREATE INDEX ix_stats_strings_id ON stats.strings (metric);
