docker build . -f Dockerfile -t dslp/default
cd ..
docker run --rm -d ^
	-p 8787:8787 ^
	-e ROOT=true -e DISABLE_AUTH=true ^
	--mount type=bind,source=%cd%,target=/home/rstudio/ ^
	dslp/default 
PAUSE