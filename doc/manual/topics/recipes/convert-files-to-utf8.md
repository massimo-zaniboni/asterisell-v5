# Convert files to UTF-8 format

Asterisell read, and import files in UTF-8 format.

This command show the character-set of a file:

```
    file -bi
```

A command for converting a file to another locale

```
    iconv -f ISO-8859-1 -t UTF-8  > t
    rm
    mv t
```

or a probably better command with in-place conversion:

```
    yum install recode
    recode ISO-8859-1..UTF8
```

