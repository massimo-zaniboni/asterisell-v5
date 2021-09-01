--  Sample employee database 
--  Copyright (C) 2007,2008, MySQL AB

USE testMySQLHaskell;

CREATE TABLE employees (
    emp_no      INT             NOT NULL,
    birth_date  DATE            NOT NULL,
    first_name  VARCHAR(14)     NOT NULL,
    last_name   VARCHAR(16)     NOT NULL,
    gender      ENUM ('M','F')  NOT NULL,    
    hire_date   DATE            NOT NULL,
    PRIMARY KEY (emp_no)
);

SELECT 'LOADING employees' as 'INFO';
source load_employees.dump ;
