create table if not exists guest (
  Id UUID not null primary key default uuid_generate_v4(),
  first_name varchar(55),
  last_name varchar(55),
  email varchar(55) not null unique,
  password varchar(255) not null unique,
  created_on timestamp
);
