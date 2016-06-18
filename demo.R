library ("dplyr")
library ("psych")

d<-read.csv("data.csv", head=TRUE)
d$sex <- factor (d$sex, labels=list("male", "female"))
d$cond <- factor (d$cond, labels=list("strong", "weak", "control"))

#人数のカウント
table (d$sex)
table (d$sex, d$cond)

#グループごとの処理
tapply(d$identity1, d$sex, mean, na.rm=TRUE)

#データのグループ化
d01<-dplyr::group_by(d,sex,cond)
dplyr::summarise(d01,mean(identity1))
dplyr::summarise(d01,sd(identity1))

dplyr::summarise(dplyr::group_by(d, sex,cond), mean(identity1))
dplyr::summarise(dplyr::group_by(d, sex,cond), sd(identity1))

#一部の人たちだけ抜き出す
d02<-d[d$sex!="female",]

#クロンバックのalpha係数を求める
identity_all<-dplyr::select(d, num_range("identity", 1:13, 1))
psych::alpha(identity_all)

psych::alpha(dplyr::select(d, num_range("identity", 1:13, 1)))

#合成変数の作成(かっこいいやり方)
d$identity_all<-apply(dplyr::select(d, num_range("identity", 1:13, 1)), 1, sum)

#合成変数の作成 (とろくさいやり方)
d$identity_all<-d$identity1+d$identity2+d$identity3+d$identity4+d$identity5+
  d$identity6+d$identity7+d$identity8+d$identity9+d$identity10+d$identity11+
  d$identity12+d$identity13

#値を変換する
d$identity_all[d$identity_all<30]<-0
d$identity_all[d$identity_all>=30]<-1

