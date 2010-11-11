<?php
/*
  +----------------------------------------------------------------------+
  | PHP Version 5                                                        |
  +----------------------------------------------------------------------+
  | Copyright (c) 1997-2004 The PHP Group                                |
  +----------------------------------------------------------------------+
  | This source file is subject to version 3.0 of the PHP license,       |
  | that is bundled with this package in the file LICENSE, and is        |
  | available through the world-wide-web at the following url:           |
  | http://www.php.net/license/3_0.txt.                                  |
  | If you did not receive a copy of the PHP license and are unable to   |
  | obtain it through the world-wide-web, please send a note to          |
  | license@php.net so we can mail you a copy immediately.               |
  +----------------------------------------------------------------------+
  | Authors:    Thomas Schöfbeck <tom@php.net>                           |
  |             Gabor Hojtsy <goba@php.net>                              |
  |             Mark Kronsbein <mk@php.net>                              |
  |             Jan Fabry <cheezy@php.net>
  +----------------------------------------------------------------------+
*/
if ($argc < 2 || $argc > 3) {
?>

Check the revision of translated files against
the actual english xml files, and print statistics

  Usage:
  <?php echo $argv[0]; ?> <language-code> [<maintainer>] [><revcheck.html>]

  <language-code> must be a valid language code used
  in the repository

  If you specify <maintainer>, the script only checks
  the files maintained by the person you add here

  If you specify ><revcheck.html>, the output is an html file.
  
  Read more about Revision comments and related
  functionality in the PHP Documentation Howto:
    http://php.net/manual/howto/translation-revtrack.html
   
  Authors: Thomas Schöfbeck <tom@php.net>
           Gabor Hojtsy <goba@php.net>
           Mark Kronsbein <mk@php.net> 
           Jan Fabry <cheezy@php.net>

<?php
  exit;
}

// Long runtime
set_time_limit(0);

// disable E_NOTICE
error_reporting(E_ALL ^ E_NOTICE);

// A file is criticaly "outdated' if
define("ALERT_REV",   10); // translation is 10 or more revisions behind the en one
define("ALERT_SIZE",   3); // translation is  3 or more kB smaller than the en one
define("ALERT_DATE", -30); // translation is 30 or more days older than the en one

// Revision marks used to flag files
define("REV_UPTODATE", 1); // actual file
define("REV_NOREV",    2); // file with revision comment without revision
define("REV_CRITICAL", 3); // criticaly old / small / outdated
define("REV_OLD",      4); // outdated file
define("REV_NOTAG",    5); // file without revision comment
define("REV_NOTRANS",  6); // file without translation

define("REV_CREDIT",   7); // only used in translators list
define("REV_WIP",      8); // only used in translators list

// Colors used to mark files by status (colors for the above types)
$CSS = array(
  REV_UPTODATE => "act",
  REV_NOREV    => "norev",
  REV_CRITICAL => "crit",
  REV_OLD      => "old",
  REV_NOTAG    => "wip",
  REV_NOTRANS  => "wip",
  REV_CREDIT   => "wip",
  REV_WIP      => "wip",
);

// Option for the link to cvs.php.net:
define('CVS_OPT', '&amp;view=patch');
define('CVS_OPT_NOWS', '&amp;view=diff&amp;diff_format=h');

// Initializing variables from parameters
$LANG = $argv[1];
if ($argc == 3) {
    $MAINT = $argv[2];
} else {
    $MAINT = "";
}

// Main directory of the PHP documentation (depends on the
// sapi used). We do need the trailing slash!
if ("cli" === php_sapi_name()) {
	if (isset($PHPDOCDIR) && is_dir($PHPDOCDIR))
		$DOCDIR = $PHPDOCDIR."/";
	else
		$DOCDIR = "./";
} else
	$DOCDIR = "../";

// =========================================================================
// Functions to get revision info and credits from a file
// =========================================================================

// Grabs the revision tag and stores credits from the file given
function get_tags($file, $val = "en-rev")
{
    // Read the first 500 chars. The comment should be at
    // the begining of the file
    $fp = @fopen($file, "r") or die ("Unable to read $file.");
    $line = fread($fp, 500);
    fclose($fp);

    // Check for English CVS revision tag (. is for $ in the preg!),
    // Return if this was needed (it should be there)
    if ($val == "en-rev") {
        preg_match("/<!-- .Revision: (\d+) . -->/", $line, $match);
        return $match[1];
    }

    // Handle credits (only if no maintainer is specified)
    if ($val == "\\S*") {
    
        global $files_by_maint;

        // Find credits info, let more credits then one,
        // using commas as list separator
        if (preg_match("'<!--\s*CREDITS:\s*(.+)\s*-->'U", $line, $match_credit)) {
          
            // Explode with commas a separators
            $credits = explode(",", $match_credit[1]);
          
            // Store all elements
            foreach ($credits as $num => $credit) {
                $files_by_maint[trim($credit)][REV_CREDIT]++;
            }
          
        }
    }

    // No match before the preg
    $match = array();
    
    // Check for the translations "revision tag"
    preg_match ("/<!--\s*EN-Revision:\s*(\d+)\s*Maintainer:\s*("
                . $val . ")\s*Status:\s*(.+)\s*-->/U", 
                $line,
                $match
    );

    // The tag with revision number is not found so search
    // for n/a revision comment (comment where revision is not known)
    if (count($match) == 0) {
        preg_match ("'<!--\s*EN-Revision:\s*(n/a)\s*Maintainer:\s*("
                    . $val . ")\s*Status:\s*(.+)\s*-->'U",
                    $line,
                    $match
        );
    }

    // Return with found revision info (number, maint, status)
    return $match;
    
} // get_tags() function end


// =========================================================================
// Functions to check file status in translated directory, and store info
// =========================================================================

// Checks a file, and gather status info
function get_file_status($file)
{
    // The information is contained in these global arrays and vars
    global $DOCDIR, $LANG, $MAINT, $files_by_mark, $files_by_maint;
    global $file_sizes_by_mark;
    global $missing_files, $missing_tags, $using_rev;

    // Transform english file name to translated file name
    $trans_file = preg_replace("'^".$DOCDIR."en/'", $DOCDIR.$LANG."/", $file);

    // If we cannot find the file, we push it into the missing files list
    if (!@file_exists($trans_file)) {
        $files_by_mark[REV_NOTRANS]++;
        $trans_name = substr($trans_file, strlen($DOCDIR) + strlen($LANG) + 1);
        $size = intval(filesize($file)/1024);
        $missing_files[$trans_name] = array( $size );
        $file_sizes_by_mark[REV_NOTRANS] += $size;
        // compute en-tags just if they're needed in the WIP-Table
        if($using_rev) {
        	$missing_files[$trans_name][] = get_tags($file);
        }
        return FALSE;
    }

    // No specific maintainer, check for a revision tag
    if (empty($MAINT)) {
        $trans_tag = get_tags($trans_file, "\\S*");
    }
    // If we need to check for a specific translator
    else {
        // Get translated files tag, with maintainer
        $trans_tag = get_tags($trans_file, $MAINT);

        // If this is a file belonging to another
        // maintainer, than we would not like to
        // deal with it anymore
        if (count($trans_tag) == 0) {
            $trans_tag = get_tags($trans_file, "\\S*");
            // We found a tag for another maintainer
            if (count($trans_tag) > 0) {
                return FALSE;
            }
        }
    }

    // Compute sizes and diffs
    $en_size    = intval(filesize($file) / 1024);
    $trans_size = intval(filesize($trans_file) / 1024);
    $size_diff  = intval($en_size) - intval($trans_size);
    
    // If we found no revision tag, then collect this
    // file in the missing tags list
    if (count($trans_tag) == 0) {
        $files_by_mark[REV_NOTAG]++;
        $file_sizes_by_mark[REV_NOTAG] += $en_size;
        $missing_tags[] = array(substr($trans_file, strlen($DOCDIR)), $en_size, $trans_size, $size_diff);
        return FALSE;
    }

    // Distribute values in separate vars for further processing
    list(, $this_rev, $this_maint, $this_status) = $trans_tag;

    // Get English file revision
    $en_rev = get_tags($file);
    
    // If we have a numeric revision number (not n/a), compute rev. diff
    if (is_numeric($this_rev)) {
        $rev_diff   = intval($en_rev) - intval($this_rev);
        $trans_rev  = $this_rev;
    } else {
        // If we have no numeric revision, make all revision
        // columns hold the rev from the translated file
        $rev_diff = $trans_rev = $this_rev;
    }

    // If the file is up-to-date
    if ($rev_diff === 0) {
        // Store file by status and maintainer
        $files_by_mark[REV_UPTODATE]++;
        $files_by_maint[$this_maint][REV_UPTODATE]++;
        $file_sizes_by_mark[REV_UPTODATE] += $en_size;
        
        return FALSE;
    } 

    // Compute times and diffs
    $en_date    = intval((time() - filemtime($file)) / 86400);
    $trans_date = intval((time() - filemtime($trans_file)) / 86400);
    $date_diff  = $en_date - $trans_date;

    // Make decision on file category by revision, date and size
    if ($rev_diff >= ALERT_REV || $size_diff >= ALERT_SIZE || $date_diff <= ALERT_DATE) {
        $status_mark = REV_CRITICAL;
    } elseif ($rev_diff === "n/a") {
        $status_mark = REV_NOREV;
    } else {
        $status_mark = REV_OLD;
    }

    // Store files by status, and by maintainer too
    $files_by_mark[$status_mark]++;
    $files_by_maint[$this_maint][$status_mark]++;
    $file_sizes_by_mark[$status_mark] += $en_size;

    return array(
        "full_name"  => $file,
        "short_name" => basename($trans_file),
        "revision"   => array($en_rev,  $trans_rev,  $rev_diff),
        "size"       => array($en_size, $trans_size, $size_diff),
        "date"       => array($en_date, $trans_date, $date_diff),
        "maintainer" => $this_maint,
        "status"     => $this_status,
        "mark"       => $status_mark
    );
    
} // get_file_status() function end

// =========================================================================
// A function to check directory status in translated directory
// =========================================================================

// Check the status of files in a diretory of smarty/doc XML files
// The English directory is passed to this function to check
function get_dir_status($dir)
{
    
    // Collect files and diretcories in these arrays
    $directories = array();
    $files       = array();
    
    // Open the directory 
    $handle = @opendir($dir);
    
    // Walk through all names in the directory
    while ($file = @readdir($handle)) {

      // If we found a file with one or two point as a name,
      // or a CVS directory, skip the file
      if (preg_match("/^\.{1,2}/",$file) || $file == 'CVS')
        continue;

      // Collect files and directories
      if (is_dir($dir.$file)) { $directories[] = $file; }
      else { $files[] = $file; }

    }
    
    // Close the directory
    @closedir($handle);
      
    // Sort files and directories
    sort($directories);
    sort($files);
      
    // Go through files first
    $dir_status = array();
    foreach ($files as $file) {
        // If the file status is OK, append the status info
        if ($file_status = get_file_status($dir.$file)) {
            $dir_status[] = $file_status;
        }
    }

    // Then go through subdirectories, merging all the info
    // coming from subdirs to one array
    foreach ($directories as $file) {
        $dir_status = array_merge(
            $dir_status, 
            get_dir_status($dir.$file.'/')
        );
    }
    
    // Return with collected file info in
    // this dir and subdirectories [if any]
    return $dir_status;

} // get_dir_status() function end


// Check for files removed in the EN tree, but still living in the translation
function get_old_files($dir)
{

    global $DOCDIR, $LANG;

    // Collect files and diretcories in these arrays
    $directories = array();
    $files       = array();

    $special_files = array(
      // french
      'LISEZ_MOI.txt',
      'TRADUCTIONS.txt',
      'Translators',
      'translation.xml'

      // todo: add all missing languages
    );

    // Open the directory
    $handle = @opendir($dir);

    // Walk through all names in the directory
    while ($file = @readdir($handle)) {

      // If we found a file with one or two point as a name,
      // or a CVS directory, skip the file
      if (preg_match("/^\.{1,2}/", $file) || $file == 'CVS')
        continue;

      // skip this files
      if (in_array($file, $special_files)) {
        continue;
      }

      // Collect files and directories
      if (is_dir($dir.$file)) {
        $directories[] = $file;
      } else {
        $files[] = $file;
      }

    }

    // Close the directory
    @closedir($handle);

    // Sort files and directories
    sort($directories);
    sort($files);

    // Go through files first
    $old_files_status = array();
    foreach ($files as $file) {

      $en_dir = preg_replace("'^".$DOCDIR.$LANG."/'", $DOCDIR."en/", $dir);

      if (!@file_exists($en_dir.$file) ) {
        $old_files_status[$dir.$file] = array(0=>intval(filesize($dir.$file)/1024));
      }

    }

    // Then go through subdirectories, merging all the info
    // coming from subdirs to one array
    foreach ($directories as $file) {
        $old_files_status = array_merge(
            $old_files_status, 
            get_old_files($dir.$file.'/')
        );
    }

    return $old_files_status;

} // get_old_files() function end


// =========================================================================
// Functions to read in the translation.xml file and process contents
// =========================================================================

// Get a multidimensional array with tag attributes
function parse_attr_string ($tags_attrs)
{
    $tag_attrs_processed = array();

    // Go through the tag attributes
    foreach($tags_attrs as $attrib_list) {

      // Get attr name and values
      preg_match_all("!(.+)=\\s*([\"'])\\s*(.+)\\2!U", $attrib_list, $attribs);

      // Assign all attributes to one associative array
      $attrib_array = array();
      foreach ($attribs[1] as $num => $attrname) {
        $attrib_array[trim($attrname)] = trim($attribs[3][$num]);
      }

      // Collect in order of tags received
      $tag_attrs_processed[] = $attrib_array;

    }

    // Retrun with collected attributes
    return $tag_attrs_processed;

} // parse_attr_string() end

// Parse the translation.xml file for
// translation related meta information
function parse_translation($DOCDIR, $LANG, $MAINT)
{
    global $files_by_mark;
    
    // Path to find translation.xml file, set default values,
    // in case we can't find the translation file
    $translation_xml = $DOCDIR.$LANG."/translation.xml";
    $output_charset  = 'UTF-8';
    $translation     = array(
        "intro"    => "",
        "persons"  => array(),
        "files"    => array(),
        "allfiles" => array(),
    );
    
    // Check for file availability, return with default
    // values, if we cannot find the file
    if (!@file_exists($translation_xml)) {
        return array($output_charset, $translation);
    }
    
    // Else go on, and load in the file, replacing all
    // space type chars with one space
    $txml = join("", file($translation_xml));
    $txml = preg_replace("/\\s+/", " ", $txml);

    // Get intro text (different for a persons info and
    // for a whole group info page)
    if (empty($MAINT)) {
        preg_match("!<intro>(.+)</intro>!s", $txml, $match);
        $translation["intro"] = trim($match[1]);
    } else {
        $translation["intro"] = "Personal Statistics for ".$MAINT;
    }
    
    // Get encoding for the output, from the translation.xml
    // file encoding (should be the same as the used encoding
    // in HTML)
    preg_match("!<\?xml(.+)\?>!U", $txml, $match);
    $xmlinfo = parse_attr_string($match);
    $output_charset = $xmlinfo[1]["encoding"];
    
    // Get persons list preg pattern, only check for a specific
    // maintainer, if the users asked for it
    if (empty($MAINT)) {
        $pattern = "!<person(.+)/\\s?>!U";
    } else {
        $pattern = "!<person([^<]+nick=\"".$MAINT."\".+)/\\s?>!U";
    }
    
    // Find all persons matching the pattern
    preg_match_all($pattern, $txml, $matches);
    $translation['persons'] = parse_attr_string($matches[1]);
    
    // Get list of work in progress files
    if (empty($MAINT)) {

        // Get all wip files
        preg_match_all("!<file(.+)/\\s?>!U", $txml, $matches);
        $translation['files'] = parse_attr_string($matches[1]);

        // Provide info about number of WIP files
        $files_by_mark[REV_WIP] += count($translation['files']);

    } else {
        
        // Only check for a specific maintainer, if we were asked to
        preg_match_all("!<file([^<]+person=\"".$MAINT."\".+)/\\s?>!U", $txml, $matches);
        $translation['files'] = parse_attr_string($matches[1]);

        // Other maintainers wip files need to be cleared from
        // available files list in the future, so store that info too.
        preg_match_all("!<file(.+)/\\s?>!U", $txml, $matches);
        $translation['allfiles'] = parse_attr_string($matches[1]);
        
        // Provide info about number of WIP files
        $files_by_mark[REV_WIP] += count($translation['allfiles']);

    }
    
    // Return with collected info in two vars
    return array($output_charset, $translation);

} // parse_translation() function end()

// =========================================================================
// Start of the program execution
// =========================================================================

// Check for directory validity
if (!@is_dir($DOCDIR . $LANG)) {
    die("The $LANG language code is not valid");
}
  
// Parse translation.xml file for more information
list($charset, $translation) = parse_translation($DOCDIR, $LANG, $MAINT);

// Add WIP files to maintainers file count and figure out,
// if we need to use optional date and revision columns
$using_date = FALSE; $using_rev = FALSE;
foreach ($translation["files"] as $num => $fileinfo) {
    $files_by_maint[$fileinfo["person"]][REV_WIP]++;
    if (isset($fileinfo["date"]))     { $using_date = TRUE; }
    if (isset($fileinfo["revision"])) { $using_rev = TRUE; }
}

// Get all files status
$files_status = get_dir_status($DOCDIR."en/");

// Get all old files in <lang> directory
$old_files = get_old_files($DOCDIR.$LANG."/");

$navbar = "<p class=c><a href=\"#intro\">Introduction</a> | " .
          "<a href=\"#translators\">Translators</a> | " .
          "<a href=\"#filesummary\">File summary by type</a> | " .
          "<a href=\"#files\">Files</a> | ";
if (count($translation["files"]) != 0)
	$navbar .= "<a href=\"#wip\">Work in progress</a> | ";
$navbar .= "<a href=\"#misstags\">Missing revision numbers</a> | " .
           "<a href=\"#missfiles\">Untranslated files</a> | " .
           "<a href=\"#oldfiles\">Old files</a></p>\n";


// Figure out generation date
$date = date("r");
  
// =========================================================================
// Start of HTML page
// =========================================================================

print <<<END_OF_MULTILINE
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd">
<html>
<head>
<title>Smarty Manual Revision-check</title>
<meta http-equiv="Content-Type" content="text/html; charset={$charset}">
<style type="text/css">
<!--
h2,td,a,p,a.ref,th { font-family:Arial,Helvetica,sans-serif; font-size:14px; }
h2,th,a.ref { color:#FFFFFF; }
td,a,p { color:#000000; }
h2     { font-size:28px; }
th     { font-weight:bold; }
.blue  { background-color:#666699; }
.act   { background-color:#68D888; }
.norev { background-color:#f4a460; }
.old   { background-color:#eee8aa; }
.crit  { background-color:#ff6347; }
.wip   { background-color:#dcdcdc; }
.r     { text-align:right }
.rb    { text-align:right; font-weight:bold; }
.c     { text-align:center }
body   { margin:0px 0px 0px 0px; background-color:#F0F0F0; }
//-->
</style>
</head>
<body>
<table width="100%" border="0" cellspacing="0" bgcolor="#666699">
<tr><td>
<table width="100%" border="0" cellspacing="1" bgcolor="#9999CC">
<tr><td><h2 class=c>Status of the translated Smarty Manual</h2><p class=c style="font-size:12px;">Generated: {$date} &nbsp; / &nbsp; Language: $LANG<br></p></td></tr>
</table>
</td></tr>
</table>
END_OF_MULTILINE;

print ($navbar);

// =========================================================================
// Intro block goes here
// =========================================================================

// If we have an introduction text, print it out, with an anchor
if (!empty($translation["intro"])) {
    print '<a name="intro"></a>';
    print '<table width="800" align="center"><tr><td class=c>' .
           $translation['intro'] . '</td></tr></table>';
}

// =========================================================================
// Translators table goes here
// =========================================================================

// If person list available (valid translation.xml file in lang), print out
// the person list, with respect to the maintainer parameter specified
if (!empty($translation["persons"])) {

print <<<END_OF_MULTILINE
<a name="translators"></a>
<table width="820" border="0" cellpadding="4" cellspacing="1" align="center">
<tr class=blue>
<th rowspan=2>Translator's name</th>
<th rowspan=2>Contact email</th>
<th rowspan=2>Nick</th>
<th rowspan=2>C<br>V<br>S</th>
<th colspan=7>Files maintained</th>
</tr>
<tr>
<th class="{$CSS[REV_CREDIT]}" style="color:#000000">cre-<br>dits</th>
<th class="{$CSS[REV_UPTODATE]}" style="color:#000000">upto-<br>date</th>
<th class="{$CSS[REV_OLD]}" style="color:#000000">old</th>
<th class="{$CSS[REV_CRITICAL]}" style="color:#000000">cri-<br>tical</th>
<th class="{$CSS[REV_NOREV]}" style="color:#000000">no<br>rev</th>
<th class="{$CSS[REV_WIP]}" style="color:#000000">wip</th>
<th class="blue">sum</th>
</tr>
END_OF_MULTILINE;

    // ' Please leave this comment here

    // We will collect the maintainers by nick here
    $maint_by_nick = array();
    
    // Print out a line for each maintainer (with respect to
    // maintainer setting provided in command line)
    foreach($translation["persons"] as $num => $person) {
        
        // Do not print out this person, if a
        // specific maintainer info is asked for
        if (!empty($MAINT) && $person["nick"] != $MAINT) {
            continue;
        }
        
        // Put maintaner number into associative array
        // [Used in further tables for referencing]
        $maint_by_nick[$person["nick"]] = $num;
        
        // Decide on the CVS text and the color of the line
        if ($person["cvs"] === "yes") {
            $cvsu = "x";
            $col = "old";
        } else {
            $cvsu = "&nbsp;";
            $col = "wip";
        }
        
        // Try to do some antispam actions
        $person["email"] = str_replace(
            "@",
            "<small>:at:</small>",
            $person["email"]
        );
        
        // Get file info for this person
        if (isset($files_by_maint[$person["nick"]])) {
            $pi = $files_by_maint[$person["nick"]];
        } else {
            $pi = array();
        }
        
        print("<tr class=$col>" .
              "<td><a name=\"maint$num\">$person[name]</a></td>" .
              "<td>$person[email]</td>" .
              "<td>$person[nick]</td>" .
              "<td class=c>$cvsu</td>" .
              "<td class=c>" . $pi[REV_CREDIT]   . "</td>" .
              "<td class=c>" . $pi[REV_UPTODATE] . "</td>" .
              "<td class=c>" . $pi[REV_OLD]      . "</td>" .
              "<td class=c>" . $pi[REV_CRITICAL] . "</td>" .
              "<td class=c>" . $pi[REV_NOREV]    . "</td>" .
              "<td class=c>" . $pi[REV_WIP]      . "</td>" .
              "<th class=blue>" . array_sum($pi) . "</th>" .
              "</tr>\n");
    }
  
    print "</table>\n<p>&nbsp;</p>\n";
} 

// =========================================================================
// Files summary table goes here
// =========================================================================

// Do not print out file summary table, if we are printing out a page
// for only one maintainer (his personal summary is in the table above)
if (empty($MAINT)) {

	print <<<END_OF_MULTILINE
<a name="filesummary"></a>
<table width="450" border="0" cellpadding="4" cellspacing="1" align="center">
<tr class=blue>
<th>File status type</th>
<th>Number of files</th>
<th>Percent of files</th>
<th>Size of files (kB)</th>
<th>Percent of size</th>
</tr>
END_OF_MULTILINE;

    $files_sum = array_sum($files_by_mark);
    $file_sizes_sum = array_sum($file_sizes_by_mark);
    
    $file_types = array(
      array (REV_UPTODATE, "Up to date files"),
      array (REV_OLD,      "Old files"),
      array (REV_CRITICAL, "Critical files"),
      array (REV_WIP,      "Work in progress"),
      array (REV_NOREV,    "Files without revision number"),
      array (REV_NOTAG,    "Files without revision tag"),
      array (REV_NOTRANS,  "Files available for translation")
    );
    
    foreach ($file_types as $num => $type) {
    	print "<tr class=".$CSS[$type[0]].">".
    		  "<td>".$type[1]."</td>".
    		  "<td class=c>".intval($files_by_mark[$type[0]])."</td>".
    		  "<td class=c>".number_format($files_by_mark[$type[0]] * 100 / $files_sum, 2 ).
    		  "%</td>".
                  "<td class=c>".intval($file_sizes_by_mark[$type[0]])."</td>".
                  "<td class=c>".number_format($file_sizes_by_mark[$type[0]] * 100 / $file_sizes_sum, 2).
                  "%</td></tr>\n";
    }

	print "<tr class=blue><th>Files total</th><th>$files_sum</th><th>100%</th><th>$file_sizes_sum</th><th>100%</th></tr>\n".
		  "</table>\n<p>&nbsp;</p>\n";

}

print ($navbar."<p>&nbsp;</p>\n");


// =========================================================================
// Files table goes here
// =========================================================================

if (count($files_status) != 0) {

print <<<END_OF_MULTILINE
<a name="files"></a>
<table width="820" border="0" cellpadding="4" cellspacing="1" align="center">
<tr class=blue>
<th rowspan=2>Translated file</th>
<th colspan=3>Revision</th>
<th colspan=3>Size in kB</th>
<th colspan=3>Age in days</th>
<th rowspan=2>Maintainer</th>
<th rowspan=2>Status</th>
</tr>
<tr class=blue>
<th>en</th>
<th>$LANG</th>
<th>diff</th>
<th>en</th>
<th>$LANG</th>
<th>diff</th>
<th>en</th>
<th>$LANG</th>
<th>diff</th>
</tr>
END_OF_MULTILINE;

    // This was the previous directory [first]
    $prev_dir = $new_dir = $DOCDIR."en";

    // Go through all files collected
    foreach ($files_status as $num => $file) {

        // Make the maintainer a link, if we have that maintainer in the list
        if (isset($maint_by_nick[$file["maintainer"]])) {
          $file["maintainer"] = '<a href="#maint' . $maint_by_nick[$file["maintainer"]] .
                                '">' . $file["maintainer"] . '</a>';
        }

        // If we have a 'numeric' revision diff and it is not zero,
        // make a link to the CVS repository's diff script
        if ($file["revision"][2] != "n/a" && $file["revision"][2] !== 0) {
            $url = 'http://code.google.com/p/smarty-php/source/diff?'
                 . 'old=' . $file['revision'][1] . '&'
                 . 'r=' . $file['revision'][0] . '&'
                 . 'format=side&'
                 . 'path=' . urlencode('/trunk/' . preg_replace("'^".$DOCDIR."'", 'docs/', $file['full_name']));

            $file['short_name'] = '<a href="' . $url . '">'. $file["short_name"] . '</a>';
        }

        // Guess the new directory from the full name of the file
        $new_dir = dirname($file["full_name"]);

        // If this is a new directory, put out old dir lines
        if ($new_dir != $prev_dir && isset($lines)) {
            echo $prev_diplay_dir;
            echo " ($line_number)</th></tr>";
    	echo $lines;
    	
    	$lines = '';
    	$line_number = 0;
            
            // Store the new actual directory
            $prev_dir = $new_dir;
        }
        // Drop out the unneeded parts from the dirname...
        $display_dir = str_replace($DOCDIR."en/", "", dirname($file["full_name"]));
        $prev_diplay_dir = "<tr class=blue><th colspan=12>$display_dir";
        
        // Save the line for the current file (get file name shorter)
        $lines .= "<tr class={$CSS[$file['mark']]}><td>{$file['short_name']}</td>".
              "<td> {$file['revision'][0]}</td>" .
              "<td> {$file['revision'][1]}</td>".
              "<td class=rb>{$file['revision'][2]} </td>".
              "<td class=r>{$file['size'][0]} </td>".
              "<td class=r>{$file['size'][1]} </td>".
              "<td class=rb>{$file['size'][2]} </td>".
              "<td class=r>{$file['date'][0]} </td>".
              "<td class=r>{$file['date'][1]} </td>".
              "<td class=rb>{$file['date'][2]} </td>".
              "<td class=c>{$file['maintainer']}</td>".
              "<td class=c>".trim($file['status'])."</td></tr>\n";
         $line_number++;
    
    }
    
    // echo the last dir and $lines
    echo "$prev_diplay_dir ($line_number)</th></tr>";
    echo $lines;
    
    print("</table>\n<p>&nbsp;</p>\n$navbar<p>&nbsp;</p>\n");
    
}


// =========================================================================
// Work in progress table goes here
// =========================================================================

// If work-in-progress list is available (valid translation.xml file in lang)
if (count($translation["files"]) != 0) {

    // Print out files table header
    print "<a name=\"wip\"></a>\n" .
    "<table width=\"820\" border=\"0\" cellpadding=\"4\" cellspacing=\"1\" align=\"center\">\n" .
    "<tr class=blue>".
    "<th>Work in progress files</th>".
    "<th>Translator</th>".
    "<th>Type</th>";
  
    // Print out date and revision columns if needed
    if ($using_date) {
        print '<th>Date</th>';
    }
    if ($using_rev) {
        print '<th>CO-Revision</th>' .
              '<th>EN-Revision</th>';
    }
    print "</tr>\n";
  
    // Go through files, and print out lines for them
    foreach($translation["files"] as $num => $finfo) {
    
        // If we have a valid maintainer, link to the summary
        if (isset($maint_by_nick[$finfo["person"]])) {
            $finfo["person"] = '<a href="#maint' . $maint_by_nick[$finfo["person"]] .
                               '">' . $finfo["person"] . '</a>';
        }
       
        // Print out the line with the first columns
        print "<tr class=wip><td>$finfo[name]</td>" .
              "<td>$finfo[person]</td><td>$finfo[type]</td>";

        // If we need the date column, print it out
        if ($using_date) {
            print "<td>$finfo[date]</td>";
        }

        // If we need the revision column, print it out
        if ($using_rev) {
            print "<td>$finfo[revision]</td><td>" .
                  $missing_files[$finfo["name"]][1] .
                  "</td>";
        }
      
        // End the line
        print "</tr>\n";

        // Collect files in WIP list
        $wip_files[$finfo["name"]] = TRUE;
    } 
  
    print "</table>\n<p>&nbsp;</p>\n$navbar<p>&nbsp;</p>\n";
    
} 

// Files translated, but without a revision comment
$count = count($missing_tags);
if ($count > 0) {
    print "<a name=\"misstags\"></a>" .
          "<table width=\"400\" border=\"0\" cellpadding=\"3\" cellspacing=\"1\" align=\"center\">\n".
          "<tr class=blue><th rowspan=2>Files without Revision-comment ($count files):</th>".
          "<th colspan=3>Sizes in kB</th></tr>\n".
          "<tr class=blue><th>en</th><th>$LANG</th><th>diff</th></tr>\n";
    foreach($missing_tags as $val) {
        // Shorten the filename (we have directory headers)
        $short_file = basename($val[0]);

        // Guess the new directory from the full name of the file
        $new_dir = dirname($val[0]);
    
        // If this is a new directory, put out dir headline
        if ($new_dir != $prev_dir) {
        
            // Print out directory header
            print "<tr class=blue><th colspan=4>$new_dir</th></tr>\n";
        
            // Store the new actual directory
            $prev_dir = $new_dir;
        }
        print "<tr class=wip><td>$short_file</td><td class=r>$val[1]</td>".
              "<td class=r>$val[2]</td><td class=r>$val[3]</td></tr>\n";
    }
    print "</table>\n<p>&nbsp;</p>\n$navbar<p>&nbsp;</p>\n";
}

// Merge all work in progress files collected
$wip_files = array_merge(
    $translation["files"],    // Files for this translator
    $translation["allfiles"]  // Files for all the translators
);

// Delete wip entires from available files list
foreach ($wip_files as $file) {
    if (isset($missing_files[$file['name']])) {
        unset($missing_files[$file['name']]);
    }
}

// Files not translated and not "wip"
$count = count($missing_files);
if ($count > 0) {
    print "<a name=\"missfiles\"></a>" .
          "<table width=\"400\" border=\"0\" cellpadding=\"3\" cellspacing=\"1\" align=\"center\">\n" .
          "<tr class=blue><th><a name=\"avail\" class=\"ref\">" .
          " Available for translation</a> ($count files):</th><th>kB</th></tr>\n";
    foreach($missing_files as $file => $info) {
        // Shorten the filename (we have directory headers)
        $short_file = basename($file);

        // Guess the new directory from the full name of the file
        $new_dir = dirname($file);
    
        // If this is a new directory, put out dir headline
        if ($new_dir != $prev_dir) {
        
            // Print out directory header if not "."
            print "<tr class=blue><th colspan=2>$new_dir</th></tr>\n";
        
            // Store the new actual directory
            $prev_dir = $new_dir;
        }

        print "<tr class=wip><td><a href=\"http://code.google.com/p/smarty-php/source/browse/trunk/docs/en/$file\">$short_file</a></td>" .
              "<td class=r>$info[0]</td></tr>\n";
    }
    print "</table>\n<p>&nbsp;</p>\n$navbar<p>&nbsp;</p>\n";

}

// Files not in EN tree
$count = count($old_files);
if ($count > 0) {
    print "<a name=\"oldfiles\"></a>" .
          "<table width=\"400\" border=\"0\" cellpadding=\"3\" cellspacing=\"1\" align=\"center\">\n" .
          "<tr class=blue><th><a name=\"notEn\" class=\"ref\">" .
          " Not in EN Tree</a> ($count files):</th><th>kB</th></tr>\n";

    foreach($old_files as $file => $info) {
        // Shorten the filename (we have directory headers)
        $short_file = basename($file);

        // Guess the new directory from the full name of the file
        $new_dir = dirname($file);
    
        // If this is a new directory, put out dir headline
        if ($new_dir != $prev_dir) {
        
            // Print out directory header if not "."
            print "<tr class=blue><th colspan=2>$new_dir</th></tr>\n";
        
            // Store the new actual directory
            $prev_dir = $new_dir;
        }

        print "<tr class=wip><td>$short_file</td>" .
              "<td class=r>$info[0]</td></tr>\n";
    }
    print "</table>\n<p>&nbsp;</p>\n$navbar<p>&nbsp;</p>\n";




}


// All OK, end the file
print "</body>\n</html>\n";

?>
