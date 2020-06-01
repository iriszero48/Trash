a=imresize(imread('Z:\原图\Car.jpg'),[500 500]);
b=imresize(imread('Z:\原图\图像.jpg'),[500 500]);
subplot(3,2,1);imshow(a);title('原图1');
subplot(3,2,2);imshow(b);title('原图2');
subplot(3,2,3);imshow(imadd(a,b));title('加');
subplot(3,2,4);imshow(imsubtract(a,b));title('减');
subplot(3,2,5);imshow(immultiply(a,b));title('乘');
subplot(3,2,6);imshow(imdivide(a,b));title('除');
