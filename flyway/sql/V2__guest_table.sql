create table if not exists guest (
  Id UUID not null primary key default uuid_generate_v4(),
  first_name varchar(50),
  last_name varchar(50),
  email varchar(50) unique,
  username varchar(50) unique not null,
  password varchar(250) not null unique,
  bio varchar(500),
  image_link varchar(250),
  created_at timestamp
);
