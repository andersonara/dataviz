#install.packages("elastic")
require(elastic)
con <- connect()


str(mtcars)
docs_bulk(con, 
          mtcars, index = "mtcars_idx")

docs_bulk(con,iris,index="iris_test")


index_exists(con, "mtcars_idx")
index_delete(con, "mtcars_idx")
index_delete(con, "iris_test")

str(mtcars)

mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)

str(mtcars)

docs_bulk(con, 
          mtcars, index = "mtcars_idf")


