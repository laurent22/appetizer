/********************************************************************************************************
* PROGRAM      : CustomAlphaWidget
* DATE - TIME  : Samstag 11 Augus 2007 
* AUTHOR       :  (Markus Künkler  )
* FILENAME     : CustomAlphaWidget.h
* LICENSE      : 
* COMMENTARY   : 
 ********************************************************************************************************/
#ifndef QAlphaWidget_H
#define QAlphaWidget_H


class  CustomAlphaWidget : public QWidget
{
     
public:
 CustomAlphaWidget(QWidget* wgtParent = 0);
 void updateAlpha(QPixmap& widgetMask);

};

#endif
