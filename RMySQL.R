# Using MySQL

library(RMySQL)

mydb = dbConnect(MySQL(), user='manoj', password='password', host='localhost')

# creating a database using RMySQL in R

dbSendQuery(mydb, "CREATE DATABASE bookstore;")

dbSendQuery(mydb, "USE bookstore")

# reconnecting to database we just created using following command in R :

mydb = dbConnect(MySQL(), user='manoj', password='password', host='localhost', dbname="bookstore")

dbSendQuery(mydb, "drop table if exists books, authors")

# creating tables in bookstore:

dbSendQuery(mydb, "
            CREATE TABLE books (
            book_id INT,
            title VARCHAR(50),
            author VARCHAR(50));")

# Show table using R:

dbListTables(mydb)

# Considering our bookstore a bit more, we realize that we need to add a few more columns for data elements: publisher, publication year, ISBN number, genre (e.g., novel, poetry, drama), description of book, etc. 
# We also realize that we want MySQL to automatically assign a number to the book_id column so that we don't have to bother creating one for each row or worry about duplicates. 
# Additionally, we've decided to change the author column from the actual author's name to an identification number that we'll join to a separate table containing a list of authors. 
# This will reduce typing, and will make sorting and searching easier, as the data will be uniform. 
# To make these alterations to the table that we've already created, enter the following SQL command through R :


dbSendQuery(mydb, "ALTER TABLE books
            CHANGE COLUMN book_id book_id INT AUTO_INCREMENT PRIMARY KEY,
            CHANGE COLUMN author author_id INT,
            ADD COLUMN description TEXT,
            ADD COLUMN genre ENUM('novel','poetry','drama', 'tutorials', 'text', 'other'),
            ADD COLUMN publisher_id INT,
            ADD COLUMN pub_year VARCHAR(4),
            ADD COLUMN isbn VARCHAR(20);")

# if R gives you an error:
#   Error in mysqlExecStatement(conn, statement, ...) : 
#   RS-DBI driver: (connection with pending rows, close resultSet before continuing)
# reconnect database using:

mydb = dbConnect(MySQL(), user='manoj', password='password', host='localhost', dbname="bookstore")

# and then run the above command of table alteration

# Now, Before moving on to adding data to our books table, let's quickly set up the authors table.

dbSendQuery(mydb, "CREATE TABLE authors
            (author_id INT AUTO_INCREMENT PRIMARY KEY,
            author_last VARCHAR(50),
            author_first VARCHAR(50),
            country VARCHAR(50));")

# Adding data into tables

dbSendQuery(mydb, "INSERT INTO authors
            (author_last, author_first, country)
            VALUES('Kumar','Manoj','India');")
# fetching last data insert id number:

last_id = fetch(dbSendQuery(mydb, "SELECT LAST_INSERT_ID();"))

# Inserting data into books table and using last insert ID number:

dbSendQuery(mydb, "INSERT INTO books
            (title, author_id, isbn, genre, pub_year)
            VALUES('R and MySQL', 1,'6900690075','tutorials','2014');")

# Note that I have not provided publisher's id and description in the above data insert, if you want you can modify that... at least try!
# Also note that we just entered for the author by using the LAST_INSERT_ID() function. you should also try:
# SELECT author_id, author_first FROM authors;
# and insert data using that....

try1 = fetch(dbSendQuery(mydb, "SELECT book_id, title, description
FROM books
                         WHERE genre = 'tutorials';"))





