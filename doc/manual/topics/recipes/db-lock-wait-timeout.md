# "Lock wait timeout exceeded" - DBMS error message

If it signaled a problem like

```
    ConnectionError {
      errFunction = \"query\",
      errNumber = 1205,
      errMessage = \"Lock wait timeout exceeded; try restarting transaction\"
    }
```

there can be pending transactions on MySQL.

Enter into the asterisell database using the root MySQL user.

```
    fab help
    fab connect:INSTANCE
    
    cat config/databases.yml
    # for seeing the database name, and admin user
    
    mysql -u root -pTHE_SAME_PASSWORD_DISPLAYED_FROM_CAT THE_DB_OF_CAT
    
    show open tables where in_use>0;
        +------------|------------------------|--------|-------------+
      | Database   | Table                  | In_use | Name_locked |
      +------------|------------------------|--------|-------------+
      | asterisell | ar_cdr                 |      1 |           0 |
      | asterisell | ar_daily_status_job    |      1 |           0 |
      | asterisell | ar_daily_status_change |      1 |           0 |
      +------------|------------------------|--------|-------------+
      3 rows in set (0.00 sec)
    
    show processlist;
      +----|-------|-----------|------------|---------|------|----------------------|------------------------------------------------------------------------------------------------------|----------+
      | Id | User  | Host      | db         | Command | Time | State                | Info                                                                                                 | Progress |
      +----|-------|-----------|------------|---------|------|----------------------|------------------------------------------------------------------------------------------------------|----------+
      | 19 | tsnet | localhost | asterisell | Sleep   |  969 |                      | NULL                                                                                                 |    0.000 |
      | 20 | tsnet | localhost | asterisell | Sleep   |  970 |                      | NULL                                                                                                 |    0.000 |
      | 21 | tsnet | localhost | asterisell | Query   |  918 | After opening tables | LOAD DATA INFILE '/var/tmp/var/www/asterisell/admin/pipe2' INTO TABLE ar_cdr  CHARACTER SET 'utf8'   |    0.000 |
      | 33 | root  | localhost | asterisell | Query   |    0 | init                 | show processlist                                                                                     |    0.000 |
      +----|-------|-----------|------------|---------|------|----------------------|------------------------------------------------------------------------------------------------------|----------+
      4 rows in set (0.00 sec)
    
    
    # kill the process with the problems
    kill 21;
```
