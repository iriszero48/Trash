a=rgb2gray(imread('Z:\原图\1.jpg'));
subplot(2,2,1);imshow(a);title('原图');
subplot(2,2,2);imshow(edge(a,'roberts'));title('roberts');
subplot(2,2,3);imshow(edge(a,'prewitt'));title('prewitt');
subplot(2,2,4);imshow(edge(a,'sobel'));title('sobel');
