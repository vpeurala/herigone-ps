CREATE TABLE association (
  id     SERIAL PRIMARY KEY,
  number VARCHAR NOT NULL,
  word   VARCHAR NOT NULL
);

INSERT INTO association (number, word) VALUES
  ('0', 'hai'),
  ('1', 'jää'),
  ('2', 'kuu'),
  ('3', 'luu');
