BEGIN;

ALTER TABLE codes ADD COLUMN "dummy" timestamp without time zone NOT NULL DEFAULT now();
UPDATE codes SET dummy = timestamp;
ALTER TABLE codes DROP COLUMN timestamp;
ALTER TABLE codes ADD COLUMN "timestamp" timestamp with time zone NOT NULL DEFAULT now();
UPDATE codes SET timestamp = CAST(dummy as timestamp with time zone);
ALTER TABLE codes DROP COLUMN dummy;

COMMIT;
