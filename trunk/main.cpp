#include "wx/wx.h" 
#include "wx/dcbuffer.h"
#include "NineSlicesPainter.h"







class MyApp: public wxApp
{
virtual bool OnInit();
};








class MyFrame: public wxFrame
{
public:

MyFrame(const wxString& title, const wxPoint& pos, const wxSize& size);

void OnQuit(wxCommandEvent& event);
void OnAbout(wxCommandEvent& event);
void OnPaint(wxPaintEvent& event);
void OnSize(wxSizeEvent& event);

DECLARE_EVENT_TABLE()
};







enum
{
ID_Quit = 1,
ID_About,
};






BEGIN_EVENT_TABLE(MyFrame, wxFrame)
EVT_MENU(ID_Quit, MyFrame::OnQuit)
EVT_MENU(ID_About, MyFrame::OnAbout)
EVT_PAINT(MyFrame::OnPaint)
EVT_SIZE(MyFrame::OnSize)
END_EVENT_TABLE()




IMPLEMENT_APP(MyApp) 



bool MyApp::OnInit()
{

// Required to enabled PNG support
wxInitAllImageHandlers();

MyFrame *frame = new MyFrame( _T("Hello World"), wxPoint(50,50), wxSize(450,340) );
frame->Show(TRUE);
frame->SetBackgroundStyle(wxBG_STYLE_CUSTOM);
SetTopWindow(frame);
return TRUE;
} 


void MyFrame::OnSize(wxSizeEvent& event)
{
	Refresh();
}



void MyFrame::OnPaint(wxPaintEvent& event)
{
	wxBufferedPaintDC dc(this);

  NineSlicesPainter nineSlices;
  nineSlices.LoadImage(wxT("Rainbow.png"));
  nineSlices.Draw(&dc, 0, 0, GetClientSize().GetWidth(), GetClientSize().GetHeight());



	//wxBitmap bmp(wxT("Rainbow.png"), wxBITMAP_TYPE_PNG);

	//wxMemoryDC sourceDC;
	//sourceDC.SelectObjectAsSource(bmp);	

	//dc.Blit(50, 50, 50, 50, &sourceDC, 0, 0);








 //   dc.SetPen(*wxBLACK_PEN);
 //   dc.SetBrush(*wxRED_BRUSH);

 //   // Get window dimensions
 //   wxSize sz = GetClientSize();

 //   // Our rectangle dimensions
 //   wxCoord w = 500, h = 500;

 //   // Center the rectangle on the window, but never
 //   // draw at a negative position.
	//int x = wxMax(0, (sz.GetWidth())/2);
 //   int y = wxMax(0, (sz.GetHeight())/2);

	//x = 0;
	//y = 0;

 //   wxRect rectToDraw(x, y, w, h);

 //   // For efficiency, do not draw if not exposed
 //   if (IsExposed(rectToDraw))
	//	dc.DrawRectangle(rectToDraw);
}










MyFrame::MyFrame(const wxString& title, const wxPoint& pos, const wxSize& size)
: wxFrame((wxFrame *)NULL, -1, title, pos, size)
{
wxMenu *menuFile = new wxMenu;

menuFile->Append( ID_About, _T("&About...") );
menuFile->AppendSeparator();
menuFile->Append( ID_Quit, _T("E&xit") );

wxMenuBar *menuBar = new wxMenuBar;
menuBar->Append( menuFile, _T("&File") );

SetMenuBar( menuBar );

CreateStatusBar();
SetStatusText( _T("Welcome to wxWidgets!") );
}
















void MyFrame::OnQuit(wxCommandEvent& WXUNUSED(event))
{
Close(TRUE);
}











void MyFrame::OnAbout(wxCommandEvent& WXUNUSED(event))
{
wxMessageBox(_T("This is a wxWidgets Hello world sample"),
_T("About Hello World"), wxOK | wxICON_INFORMATION, this);
}












