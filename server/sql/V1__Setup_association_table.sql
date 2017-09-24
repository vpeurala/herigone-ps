CREATE TABLE association (
  id     BIGSERIAL PRIMARY KEY,
	number VARCHAR NOT NULL,
  word   VARCHAR NOT NULL
);

INSERT INTO association (number, word) VALUES
  ('0', 'hai'),
	('1', 'jää');
