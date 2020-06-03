a=rgb2gray(imread('Z:\原图\1.jpg'));
subplot(2,2,1);imshow(a);title('原图');
subplot(2,2,2);imshow(imfilter(a,fspecial('laplacian')));title('laplacian');
subplot(2,2,3);imshow(imfilter(a,fspecial('prewitt')));title('prewitt');
subplot(2,2,4);imshow(imfilter(a,fspecial('sobel')));title('sobel');
