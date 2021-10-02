create table if not exists user_following (
  Id serial,
  user_id UUID not null references guest (Id),
  following_user_id UUID unique references guest (Id),
  created_at timestamp
)
