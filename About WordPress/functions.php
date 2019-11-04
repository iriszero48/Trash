/**
 * Login Logo.
 */
function custom_login_logo() {
	echo '<style type="text/css">h1 a { background-image:url('.get_bloginfo('template_directory').'/login-logo.png) !important; }</style>';
}
add_action('login_head', 'custom_login_logo');

/**
 * Remove Version.
 */
function left_admin_footer($text) {
	$text = 'Hello, World! ';
	return $text;
}
function right_admin_footer($text) {
}
remove_action( 'wp_head', 'wp_generator' ) ;
remove_action( 'wp_head', 'wlwmanifest_link' ) ;
remove_action( 'wp_head', 'rsd_link' ) ;
add_filter('admin_footer_text', 'left_admin_footer');
add_filter('update_footer', 'right_admin_footer', 11);

/**
 * Remove their logo 
 */
function annointed_admin_bar_remove() {
	global $wp_admin_bar;
	$wp_admin_bar->remove_menu('wp-logo');
}
add_action('wp_before_admin_bar_render', 'annointed_admin_bar_remove', 0);

/**
 * Disable sw.org
 */
function remove_dns_prefetch( $hints, $relation_type ) {
if ( 'dns-prefetch' === $relation_type ) {
return array_diff( wp_dependencies_unique_hosts(), $hints );
}
return $hints;
}
add_filter( 'wp_resource_hints', 'remove_dns_prefetch', 10, 2 );

/**
 * Remove api.w.org
 */
add_filter('rest_enabled', '_return_false');
add_filter('rest_jsonp_enabled', '_return_false');
remove_action('wp_head', 'rest_output_link_wp_head', 10 );
remove_action('wp_head', 'wp_oembed_add_discovery_links', 10 );

/**
 * Remove src version.
 */
function remove_src_version( $src ){
    return remove_query_arg( 'ver', $src );
    }
add_filter( 'script_loader_src', 'remove_src_version', 15, 1 );
add_filter( 'style_loader_src', 'remove_src_version', 15, 1 );

add_action('login_enqueue_scripts','login_protection');  
function login_protection(){  
    if($_GET['test'] != 'test')header('Location: https://www.test.com/');  
}
