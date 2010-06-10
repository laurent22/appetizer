/********************************************************************************************************
* PROGRAM      : CustomAlphaWidget
* DATE - TIME  : Samstag 11 Augus 2007 
* AUTHOR       :  (Markus Künkler )
* FILENAME     : CustomAlphaWidget.cpp
* LICENSE      : 
* COMMENTARY   : 
 ********************************************************************************************************/
#include <stable.h>


#include "CustomAlphaWidget.h"

CustomAlphaWidget::CustomAlphaWidget( QWidget* wgtParent)
 : QWidget(wgtParent)    
{
 setWindowFlags(Qt::FramelessWindowHint | Qt::Widget);
 resize(500,500);
 SetWindowLong(winId(), 
               GWL_EXSTYLE, 
               GetWindowLong(winId(), GWL_EXSTYLE) | WS_EX_LAYERED);	
}

void CustomAlphaWidget::updateAlpha(QPixmap& widgetMask)
{	
 HBITMAP oldBitmap;
 HBITMAP hBitmap;	
 SIZE size;
 size.cx = widgetMask.width();
 size.cy = widgetMask.height();
 HDC screenDc = GetDC(NULL);
 POINT pointSource;
 pointSource.x = 0;
 pointSource.y = 0; 
 POINT topPos;
 topPos.x = x();
 topPos.y = y();	
 HDC memDc = CreateCompatibleDC(screenDc);
 BLENDFUNCTION blend;
 blend.BlendOp             = AC_SRC_OVER;
 blend.BlendFlags          = 0;
 blend.SourceConstantAlpha = 255;
 blend.AlphaFormat         = AC_SRC_ALPHA;
 hBitmap = widgetMask.toWinHBITMAP(QPixmap::PremultipliedAlpha); 
 oldBitmap = (HBITMAP)SelectObject(memDc, hBitmap);
 UpdateLayeredWindow(winId(), screenDc,  &topPos,  &size, memDc,  &pointSource, 0, &blend, ULW_ALPHA);
 ReleaseDC( NULL, screenDc);
 if (hBitmap!=NULL) {
   SelectObject(memDc, oldBitmap);
   DeleteObject(hBitmap); 
   DeleteObject(hBitmap);
 }
 DeleteDC(memDc); 
}