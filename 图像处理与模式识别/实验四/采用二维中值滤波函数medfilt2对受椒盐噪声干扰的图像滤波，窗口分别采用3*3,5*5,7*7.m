a=imnoise(rgb2gray(imread('Z:\原图\1.jpg')),'salt & pepper',0.04);
subplot(2,2,1);imshow(a);title('椒盐噪声');
subplot(2,2,2);imshow(medfilt2(a,[3 3]));title('3*3');
subplot(2,2,3);imshow(medfilt2(a,[5 5]));title('5*5');
subplot(2,2,4);imshow(medfilt2(a,[7 7]));title('7*7');
