clear, close all

ImageName               =   'fig1.png';
ImageDirectory          =   '~/Desktop/';

pdt                     =   'protanopic';     %String 'protanopic' or 'deuteranopic' or 'tritanopic' indicating
                                                %type of colour blindness to simulate.

%% LOAD IMAGE
ParentImage         = imread([ImageDirectory,ImageName]);

rgbblind = colourblind(ParentImage, pdt);

imagesc(rgbblind)
axis equal
axis tight
axis off
