#include "wx/wx.h" 
#include "wx/dcbuffer.h"
#include "NineSlicesPainter.h"
#include "MainFrame.h"
#include "wx/sysopt.h"



class MyApp: public wxApp
{
virtual bool OnInit();
};











//enum
//{
//ID_Quit = 1,
//ID_About,
//};





IMPLEMENT_APP(MyApp) 



bool MyApp::OnInit()
{

// Required to enabled PNG support
wxInitAllImageHandlers();

wxSystemOptions::SetOption(wxT("msw.window.no-clip-children"),wxT("1"));

MainFrame *frame = new MainFrame();
frame->Show(true);

SetTopWindow(frame);

//MyFrame *frame = new MyFrame( _T("Hello World"), wxPoint(50,50), wxSize(450,340) );
//frame->Show(TRUE);
//frame->SetBackgroundStyle(wxBG_STYLE_CUSTOM);
//SetTopWindow(frame);
return true;
} 


//void MyFrame::OnSize(wxSizeEvent& event)
//{
//	Refresh();
//}
//
//
//
//void MyFrame::OnPaint(wxPaintEvent& event)
//{
//	wxBufferedPaintDC dc(this);
//
//  NineSlicesPainter nineSlices;
//  nineSlices.LoadImage(wxT("Rainbow.png"));
//  nineSlices.Draw(&dc, 0, 0, GetClientSize().GetWidth(), GetClientSize().GetHeight());
//}
//
//
//
//
//
//
//
//
//
//
//MyFrame::MyFrame(const wxString& title, const wxPoint& pos, const wxSize& size)
//: wxFrame((wxFrame *)NULL, -1, title, pos, size)
//{
//wxMenu *menuFile = new wxMenu;
//
//menuFile->Append( ID_About, _T("&About...") );
//menuFile->AppendSeparator();
//menuFile->Append( ID_Quit, _T("E&xit") );
//
//wxMenuBar *menuBar = new wxMenuBar;
//menuBar->Append( menuFile, _T("&File") );
//
//SetMenuBar( menuBar );
//
//CreateStatusBar();
//SetStatusText( _T("Welcome to wxWidgets!") );
//}
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//void MyFrame::OnQuit(wxCommandEvent& WXUNUSED(event))
//{
//Close(TRUE);
//}
//
//
//
//
//
//
//
//
//
//
//
//void MyFrame::OnAbout(wxCommandEvent& WXUNUSED(event))
//{
//wxMessageBox(_T("This is a wxWidgets Hello world sample"),
//_T("About Hello World"), wxOK | wxICON_INFORMATION, this);
//}