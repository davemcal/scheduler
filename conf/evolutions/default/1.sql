# Tasks schema
 
# --- !Ups

CREATE SEQUENCE employee_id_seq;
CREATE TABLE employee (
    e_id integer NOT NULL DEFAULT nextval('employee_id_seq'),
    e_name varchar(255)
);
 
# --- !Downs
 
DROP TABLE employee;
DROP SEQUENCE employee_id_seq;