package events {	

/**
 * Dispatched when a menu is about to be displayed
 */

public class MenuOpening extends Event {
	
	/**
	 * The menu that is about to be displayed. You can append
	 * additional items to it.
	 */
	public var menu:Menu;
	
}
}