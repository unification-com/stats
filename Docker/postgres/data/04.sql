DROP TABLE IF EXISTS stats.metricsf;

CREATE TABLE stats.metricsf (
    id SERIAL NOT NULL,
    metric TEXT NOT NULL,
    feature TEXT NOT NULL,
    sample DOUBLE PRECISION NOT NULL,
    utc_date TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    PRIMARY KEY (id)
)

;

CREATE INDEX ix_stats_metricsf_id ON stats.metricsf (metric);
