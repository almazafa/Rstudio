rm(list=ls())
Ratings=read.csv("C:/Users/almaz/AppData/Local/Packages/61954lingguang.10202F78DD1FD_5b5h4n7n89bzm/IMDB/archive/title.ratings.tsv/title.ratings.csv")

Row_Number = max(averageRating$tconst)
Number_Unique_Users = length(unique(averageRating$tconst))
Row_Number == Number_Unique_Users
Users_Unique = unique(averageRating$tconst)

Col_Number = max(averageRating$numVotes)
Number_Unique_Items = length(unique(averageRating$numVotes))
Col_Number == Number_Unique_Items
Items_Unique = unique(averageRating$numVotes)

Mat_Ratings=matrix(NA,
                   nrow = Number_Unique_Users,
                   ncol = Number_Unique_Items)

rownames(Mat_Ratings)=paste0("user_",Users_Unique)
colnames(Mat_Ratings)=paste0("item_",Items_Unique)

for (i in 1:nrow(averageRating)){
  Mat_Ratings[which(averageRating$tconst[i]==Users_Unique),
              which(averageRating$numVotes[i]==Items_Unique)]=
    averageRating$Rating[i]
}

Ratings_Counts=NA
for (i in 1:nrow(Mat_Ratings))
  Ratings_Counts[i]=length(which(Mat_Ratings[i,]>0))
Mat_Ratings=cbind(Mat_Ratings,Ratings_Counts)

sort_mat_rating=Mat_Ratings[order(Mat_Ratings[,486], decreasing = TRUE),]

sort_maMat_Ratings=sort_mat_rating[1:3,1:486]
Ratings_Counts=NA
for (i in 1:ncol(Mat_Ratings_100))
  Ratings_Counts[i]=length(which(Mat_Ratings_100[,i]>0))
Mat_Ratings_100=rbind(Ratings_Counts,Mat_Ratings_100)
Mat_Ratings_100=Mat_Ratings_100[,order(Mat_Ratings_100[1,], decreasing = TRUE)]
Mat_Ratings_100=Mat_Ratings_100[2:101,1:100]
