DROP TABLE IF EXISTS stats.metrics;
DROP TABLE IF EXISTS stats.markers;

CREATE TABLE stats.metrics (
    id SERIAL NOT NULL,
    metric TEXT NOT NULL,
    feature TEXT NOT NULL,
    sample BIGINT NOT NULL,
    utc_date TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    PRIMARY KEY (id)
)

;

CREATE INDEX ix_stats_metrics_id ON stats.metrics (metric);


CREATE TABLE stats.markers (
    id SERIAL NOT NULL,
    marker TEXT NOT NULL,
    utc_date TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    PRIMARY KEY (id)
)

;