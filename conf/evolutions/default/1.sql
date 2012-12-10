# Tasks schema
 
# --- !Ups

CREATE SEQUENCE employee_id_seq;
CREATE TABLE employee (
    eid integer NOT NULL DEFAULT nextval('employee_id_seq'),
    ename varchar(255),
    estart int,
    eend int,
    eispharmacist boolean
);

# --- !Downs
 
DROP TABLE employee;
DROP SEQUENCE employee_id_seq;