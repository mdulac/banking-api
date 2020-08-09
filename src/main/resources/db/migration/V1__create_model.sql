create table companies
(
    id   uuid primary key,
    name text not null
);

create table users
(
    id         uuid primary key,
    company_id uuid not null references companies on delete restrict
);

create table wallets
(
    id         uuid primary key,
    balance    decimal    not null,
    currency   varchar(3) not null,
    company_id uuid       not null references companies on delete restrict,
    is_master  boolean    not null
);

create table cards
(
    id              uuid primary key,
    wallet_id       uuid        not null references wallets on delete restrict,
    currency        varchar(3)  not null,
    balance         decimal     not null,
    number          varchar(16) not null,
    expiration_date timestamp   not null,
    ccv             varchar(3)  not null,
    user_id         uuid        not null references users on delete restrict,
    is_blocked      boolean     not null
);

create table transfers
(
    id                 uuid primary key,
    timestamp          timestamp  not null,
    amount             decimal    not null,
    origin_currency    varchar(3) not null,
    target_currency    varchar(3) not null,
    conversion_fee     decimal,
    origin_entity_id   uuid       not null,
    origin_entity_type text       not null,
    target_entity_id   uuid       not null,
    target_entity_type text       not null
);