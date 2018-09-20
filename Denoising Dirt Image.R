# libraries
library("pacman")
pacman::p_load(png, raster, data.table, reshape)

dirtyFolder = "../input/train"
cleanFolder = "../input/train_cleaned"

dat = NULL
filenames = list.files(dirtyFolder)
for (f in filenames)
{
  print(f)
  imgX = readPNG(file.path(dirtyFolder, f))
  imgY = readPNG(file.path(cleanFolder, f))
  
  # turn the images into vectors
  x = matrix(imgX, nrow(imgX) * ncol(imgX), 1)
  y = matrix(imgY, nrow(imgY) * ncol(imgY), 1)
  
  if (f == filenames[1])
  {
    dat = cbind(y, x)
    names(dat) = NULL
  } else
  {
    dat = rbind(cbind(y, x))
  }
}
dat = data.frame(dat)
names(dat) = c("y", "x")

lm.mod.1 = lm(y ~ x, data = dat[y > 0.05 & y < 0.95,])

dirtyFolder = "../input/test"
filenames = list.files(dirtyFolder)
for (f in filenames)
{
  print(f)
  imgX = readPNG(file.path(dirtyFolder, f))
  x = matrix(imgX, nrow(imgX) * ncol(imgX), 1)
  y = coef(lm.mod.1)[1] + coef(lm.mod.1)[2] * x
  y[y < 0] = 0
  y[y > 1] = 1
  img = matrix(y, nrow(imgX), ncol(imgX))
  img.dt=data.table(melt(img))
  names.dt<-names(img.dt)
  setnames(img.dt,names.dt[1],"X1")
  setnames(img.dt,names.dt[2],"X2")
  Numfile = gsub(".png", "", f, fixed=TRUE)
  img.dt[,id:=paste(Numfile,X1,X2,sep="_")]
  write.table(img.dt[,c("id","value"),with=FALSE], file = "submission.csv", sep = ",", col.names = (f == filenames[1]),row.names = FALSE,quote = FALSE,append=(f != filenames[1]))
  
  # show a sample
  if (f == "4.png")
  {
    writePNG(imgX, "train_101.png")
    writePNG(img, "train_cleaned_101.png")
  }
}