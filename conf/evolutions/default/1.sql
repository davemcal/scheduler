# Tasks schema
 
# --- !Ups

CREATE SEQUENCE employee_id_seq;
CREATE TABLE employee (
    id integer NOT NULL DEFAULT nextval('employee_id_seq'),
    name varchar(255),
    start int,
    end int,
    ispharmacist boolean
);
 
# --- !Downs
 
DROP TABLE employee;
DROP SEQUENCE employee_id_seq;