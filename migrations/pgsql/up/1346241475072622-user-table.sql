CREATE TABLE users(id SERIAL PRIMARY KEY,
                   email TEXT UNIQUE NOT NULL,
                   username TEXT UNIQUE NOT NULL,
                   password TEXT NOT NULL,
                   salt TEXT NOT NULL,
                   date_of_birth DATE)