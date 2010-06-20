/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_FloatingButton_H
#define Appetizer_FloatingButton_H

#include <GraphicsItem.h>
#include <GraphicsWindow.h>

namespace appetizer {

class FloatingButton : public GraphicsItem {

public:

  FloatingButton(GraphicsWindow* parentWindow);
  void applySkin();
  inline QString action() const { return action_; }
  void setAction(const QString& action);
  int defaultWidth() const;
  int defaultHeight() const;
  
protected:

	void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

private:
	
	QPixmap background_;
	QPixmap icon_;
  QString action_;

};

}
#endif // Appetizer_FloatingButton_H
