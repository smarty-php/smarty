<?php

class Smarty_Compiler extends Smarty {

    // internal vars
    var $_sectionelse_stack     =   array();    // keeps track of whether section had 'else' part
    var $_literal_blocks        =   array();    // keeps literal template blocks
    var $_current_file          =   null;       // the current template being compiled
    var $_current_line_no       =   1;          // line number for error messages
        
/*======================================================================*\
    Function:   _traverse_files()
    Purpose:    traverse the template files & process each one
\*======================================================================*/
    function _traverse_files($tpl_dir, $depth)
    {
        $retval = true;

        if (is_dir($tpl_dir)) {
            $dir_handle = opendir($tpl_dir);
            while ($curr_file = readdir($dir_handle)) {
                if ($curr_file == '.' || $curr_file == '..')
                    continue;

                $filepath = $tpl_dir.'/'.$curr_file;
                if (is_readable($filepath)) {
                    if (is_file($filepath) && substr($curr_file, -strlen($this->tpl_file_ext)) == $this->tpl_file_ext) {
                        if (!$this->_process_file($filepath)) {
                            $retval = false;
                            break;
                        }
                    } else if (is_dir($filepath)) {
                        if (!$this->_traverse_files($filepath, $depth + 1)) {
                            $retval = false;
                            break;
                        }
                    } else {
                        // invalid file type, skipping
                        $this->_set_error_msg("Invalid filetype for $filepath, skipping");
                        continue;
                    }
                }
            }

            closedir($dir_handle);
            return $retval;
        } else {
            $this->_set_error_msg("Directory \"$tpl_dir\" does not exist or is not a directory.");
            return false;
        }
    }

/*======================================================================*\
    Function:   _process_file()
    Input:      test template files for modifications
                and execute the compilation for each
                one requiring it.
\*======================================================================*/
    function _process_file($filepath)
    {
        if(preg_match("/^(.+)\/([^\/]+)$/", $filepath, $match)) {
            $tpl_file_dir = $match[1];          
            $tpl_file_name = $match[2] . '.php';

            $compile_dir = preg_replace('!^' . preg_quote($this->template_dir, '!') . '!',
                                        $this->compile_dir, $match[1]);

            //create directory if none exists
            $this->_create_dir_structure($compile_dir);

            // compile the template file if none exists or has been modified or recompile is forced
            if ($this->force_compile || !file_exists($compile_dir."/".$tpl_file_name) ||
                ($this->_modified_file($filepath, $compile_dir."/".$tpl_file_name))) {
                if (!$this->_compile_file($filepath, $compile_dir."/".$tpl_file_name))
                    return false;
            } else {
                // no compilation needed
                return true;
            }
        } else {
            $this->_set_error_msg("problem matching \"$filepath.\"");
            return false;
        }

        return true;
    }

/*======================================================================*\
    Function:   _modified_file()
    Input:      return comparison of modification times of files
\*======================================================================*/
    function _modified_file($filepath, $compilepath)
    {
        if (filemtime($filepath) >= filemtime($compilepath))
            return true;
        return false;
    }

/*======================================================================*\
    Function:   _compile_file()
    Input:      compile a template file
\*======================================================================*/
    function _compile_file($filepath, $compilepath)
    {
        if (!($template_contents = $this->_read_file($filepath)))
            return false;

        $this->_current_file = str_replace($this->template_dir . '/', '', $filepath);
        $this->_current_line_no = 1;
        $ldq = preg_quote($this->left_delimiter, '!');
        $rdq = preg_quote($this->right_delimiter, '!');

        /* Pull out the literal blocks. */
        preg_match_all("!{$ldq}literal{$rdq}(.*?){$ldq}/literal{$rdq}!s", $template_contents, $match);
        $this->_literal_blocks = $match[1];
        $template_contents = preg_replace("!{$ldq}literal{$rdq}(.*?){$ldq}/literal{$rdq}!s",
                                        $this->quote_replace($this->left_delimiter.'literal'.$this->right_delimiter), $template_contents);

        /* Gather all template tags. */
        preg_match_all("!{$ldq}\s*(.*?)\s*{$rdq}!s", $template_contents, $match);
        $template_tags = $match[1];
        /* Split content by template tags to obtain non-template content. */
        $text_blocks = preg_split("!{$ldq}.*?{$rdq}!s", $template_contents);

        /* TODO: speed up the following with preg_replace and /F once we require that version of PHP */

        /* loop through text blocks */
        for ($curr_tb = 0; $curr_tb <= count($text_blocks); $curr_tb++) {
            /* match anything within <? ?> */
            if (preg_match_all('!(<\?[^?]*?\?>|<script\s+language\s*=\s*[\"\']?php[\"\']?\s*>)!is', $text_blocks[$curr_tb], $sp_match)) {
                /* found at least one match, loop through each one */
                for ($curr_sp = 0; $curr_sp < count($sp_match[0]); $curr_sp++) {
                    if (preg_match('!^(<\?(php\s|\s|=\s)|<script\s*language\s*=\s*[\"\']?php[\"\']?\s*>)!is', $sp_match[0][$curr_sp])) {
                        /* php tag */
                        if ($this->php_handling == SMARTY_PHP_PASSTHRU) {
                            /* echo php contents */
                            $text_blocks[$curr_tb] = str_replace($sp_match[0][$curr_sp], '<?php echo \''.str_replace("'", "\'", $sp_match[0][$curr_sp]).'\'; ?>'."\n", $text_blocks[$curr_tb]);
                       } else if ($this->php_handling == SMARTY_PHP_QUOTE) {
                            /* quote php tags */
                            $text_blocks[$curr_tb] = str_replace($sp_match[0][$curr_sp], htmlspecialchars($sp_match[0][$curr_sp]), $text_blocks[$curr_tb]);
                        } else if ($this->php_handling == SMARTY_PHP_REMOVE) {
                            /* remove php tags */
                            if (substr($sp_match[0][$curr_sp], 0, 2) == '<?')
                                $text_blocks[$curr_tb] = str_replace($sp_match[0][$curr_sp], '', $text_blocks[$curr_tb]);
                            else
                                /* attempt to remove everything between <script ...> and </script> */
                                $text_blocks[$curr_tb] = preg_replace('!'.preg_quote($sp_match[0][$curr_sp], '!').'.*?</script\s*>!is', '', $text_blocks[$curr_tb]);
                        }
                    } else
                        /* echo the non-php tags */
                        $text_blocks[$curr_tb] = str_replace($sp_match[0][$curr_sp], '<?php echo \''.str_replace("'", "\'", $sp_match[0][$curr_sp]).'\'; ?>'."\n", $text_blocks[$curr_tb]);
                }
            }
        }

        /* Compile the template tags into PHP code. */
        $compiled_tags = array();
        for ($i = 0; $i < count($template_tags); $i++) {
            $this->_current_line_no += substr_count($text_blocks[$i], "\n");
            $compiled_tags[] = $this->_compile_tag($template_tags[$i]);
            $this->_current_line_no += substr_count($template_tags[$i], "\n");
        }

        $compiled_contents = '';

        /* Interleave the compiled contents and text blocks to get the final result. */
        for ($i = 0; $i < count($compiled_tags); $i++) {
            $compiled_contents .= $text_blocks[$i].$compiled_tags[$i];
        }
        $compiled_contents .= $text_blocks[$i];

        /* Reformat data between 'strip' and '/strip' tags, removing spaces, tabs and newlines. */
        if (preg_match_all("!{$ldq}strip{$rdq}.*?{$ldq}/strip{$rdq}!s", $compiled_contents, $match)) {
            $strip_tags = $match[0];
            $strip_tags_modified = preg_replace("!{$ldq}/?strip{$rdq}|[\t ]+$|^[\t ]+!m", '', $strip_tags);
            $strip_tags_modified = preg_replace('![\r\n]+!m', '', $strip_tags_modified);
            for ($i = 0; $i < count($strip_tags); $i++)
                $compiled_contents = preg_replace("!{$ldq}strip{$rdq}.*?{$ldq}/strip{$rdq}!s",
                                                  $strip_tags_modified[$i], $compiled_contents, 1);
        }

        if(!$this->_write_file($compilepath, $compiled_contents))
            return false;

        return true;
    }

/*======================================================================*\
    Function: _process_cached_inserts
    Purpose:  Replace cached inserts with the actual results
\*======================================================================*/
    function _process_cached_inserts($results)
    {
        preg_match_all('!'.$this->_smarty_md5.'{insert_cache (.*)}'.$this->_smarty_md5.'!Uis',
                       $results, $match);
        list($cached_inserts, $insert_args) = $match;

        for ($i = 0; $i < count($cached_inserts); $i++) {
            $args = unserialize($insert_args[$i]);
            $name = $args['name'];
            unset($args['name']);

            $function_name = 'insert_' . $name;
            $replace = $function_name($args);

            $results = str_replace($cached_inserts[$i], $replace, $results);
        }

        return $results;
    }   

/*======================================================================*\
    Function: _compile_tag
    Purpose:  Compile a template tag
\*======================================================================*/
    function _compile_tag($template_tag)
    {
        /* Matched comment. */
        if ($template_tag{0} == '*' && $template_tag{strlen($template_tag)-1} == '*')
            return '';

        $qstr_regexp = '"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"|\'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'';

        /* Split tag into two parts: command and the arguments. */
        preg_match('/^(
                       (?: ' . $qstr_regexp . ' | (?>[^"\'\s]+))+
                      )
                      (?:\s+(.*))?
                    /xs', $template_tag, $match);
        list(, $tag_command, $tag_args) = $match;

        /* If the tag name matches a variable or section property definition,
           we simply process it. */
        if (preg_match('!^\$(\w+(\.\w+)?/)*\w+(?>\.\w+)*(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|]+))*)*$!', $tag_command) ||   // if a variable
            preg_match('!^#(\w+)#(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|]+))*)*$!', $tag_command)     ||  // or a configuration variable
            preg_match('!^%\w+\.\w+%(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|]+))*)*$!', $tag_command)) {    // or a section property
            settype($tag_command, 'array');
            $this->_parse_vars_props($tag_command);
            return "<?php echo $tag_command[0]; ?>\n";
        }

        switch ($tag_command) {
            case 'include':
                return $this->_compile_include_tag($tag_args);

            case 'if':
                return $this->_compile_if_tag($tag_args);

            case 'else':
                return '<?php else: ?>';

            case 'elseif':
                return $this->_compile_if_tag($tag_args, true);

            case '/if':
                return '<?php endif; ?>';

            case 'ldelim':
                return $this->left_delimiter;

            case 'rdelim':
                return $this->right_delimiter;

            case 'section':
                array_push($this->_sectionelse_stack, false);
                return $this->_compile_section_start($tag_args);

            case 'sectionelse':
                $this->_sectionelse_stack[count($this->_sectionelse_stack)-1] = true;
                return "<?php endfor; else: ?>";

            case '/section':
                if (array_pop($this->_sectionelse_stack))
                    return "<?php endif; ?>";
                else
                    return "<?php endfor; endif; ?>";

            case 'config_load':
                return $this->_compile_config_load_tag($tag_args);

            case 'strip':
            case '/strip':
                return $this->left_delimiter.$tag_command.$this->right_delimiter;

            case 'literal':
                list (,$literal_block) = each($this->_literal_blocks);
                $this->_current_line_no += substr_count($literal_block, "\n");
                return "<?php echo '".str_replace("'","\'",$literal_block)."'; ?>\n";

            case 'insert':
                return $this->_compile_insert_tag($tag_args);

            default:
                if (isset($this->custom_funcs[$tag_command])) {
                    return $this->_compile_custom_tag($tag_command, $tag_args);
                } else {
                    $this->_syntax_error("unknown tag - '$tag_command'", E_USER_WARNING);
                    return;
                }
        }
    }

/*======================================================================*\
    Function: _compile_custom_tag
    Purpose:  compile custom tag
\*======================================================================*/
    function _compile_custom_tag($tag_command, $tag_args)
    {
        $function = $this->custom_funcs[$tag_command];

        if (!function_exists($function)) {
            $this->_syntax_error("custom function '$tag_command' is not implemented", E_USER_WARNING);
            return;
        }

        $attrs = $this->_parse_attrs($tag_args);
        foreach ($attrs as $arg_name => $arg_value) {
            if (is_bool($arg_value))
                $arg_value = $arg_value ? 'true' : 'false';
            $arg_list[] = "'$arg_name' => $arg_value";
        }

        return "<?php $function(array(".implode(',', (array)$arg_list).")); ?>";
    }

/*======================================================================*\
    Function: _compile_insert_tag
    Purpose:  Compile {insert ...} tag
\*======================================================================*/
    function _compile_insert_tag($tag_args)
    {
        $attrs = $this->_parse_attrs($tag_args);
        $name = substr($attrs['name'], 1, -1);

        if (empty($name)) {
            $this->_syntax_error("missing insert name");
        }

        foreach ($attrs as $arg_name => $arg_value) {
            if (is_bool($arg_value))
                $arg_value = $arg_value ? 'true' : 'false';
            $arg_list[] = "'$arg_name' => $arg_value";
        }

        return "<?php echo _smarty_insert_handler(array(".implode(', ', (array)$arg_list)."), \$this->caching, \$this->_smarty_md5); ?>\n";
    }   
 

/*======================================================================*\
    Function: _compile_config_load_tag
    Purpose:  Compile {config_load ...} tag
\*======================================================================*/
    function _compile_config_load_tag($tag_args)
    {
        $attrs = $this->_parse_attrs($tag_args);

        if (empty($attrs['file'])) {
            $this->_syntax_error("missing 'file' attribute in config_load tag");
        }

        $output  = "<?php if (!class_exists('Config_File'))\n" .
                   "    include_once 'Config_File.class.php';\n" .
                   "if (!is_object(\$_conf_obj) || get_class(\$_conf_obj) != 'config_file') {\n" .
                   "    \$_conf_obj  = new Config_File('".$this->config_dir."');\n" .
                   "}\n" .
                   "\$_config = array_merge((array)\$_config, \$_conf_obj->get(".$attrs['file']."));\n";

        if (!empty($attrs['section']))
            $output .= '$_config = array_merge((array)$_config, $_conf_obj->get('.$attrs['file'].', '.$attrs['section'].')); ';

        $output .= '?>';

        return $output;
    }

/*======================================================================*\
    Function: _compile_include_tag
    Purpose:  Compile {include ...} tag
\*======================================================================*/
    function _compile_include_tag($tag_args)
    {
        $attrs = $this->_parse_attrs($tag_args);

        if (empty($attrs['file'])) {
            $this->_syntax_error("missing 'file' attribute in include tag");
        } else
            $attrs['file'] = $this->_dequote($attrs['file']);

        if (count($attrs) > 1) {
            $include_func_name = uniqid("_include_");
            $include_file_name = $this->compile_dir.'/'.$attrs['file'];

            foreach ($attrs as $arg_name => $arg_value) {
                if ($arg_name == 'file') continue;
                if (is_bool($arg_value))
                    $arg_value = $arg_value ? 'true' : 'false';
                $arg_list[] = "'$arg_name' => $arg_value";
            }

            return  "<?php " .
                    "if (!function_exists('$include_func_name')) {\n".
                    "   function $include_func_name(\$file_name, \$def_vars, \$include_vars)\n" .
                    "   {\n" .
                    "       extract(\$def_vars);\n" .
                    "       extract(\$include_vars);\n" .
                    "       include \"\$file_name.php\";\n" .
                    "   }\n" .
                    "}\n" .
                    "$include_func_name(\"$include_file_name\", get_defined_vars(), array(".implode(',', (array)$arg_list)."));\n?>";
        } else
             return '<?php include "'.$this->compile_dir.'/'.$attrs['file'].'.php"; ?>';
    }

/*======================================================================*\
    Function: _compile_section_start
    Purpose:  Compile {section ...} tag
\*======================================================================*/
    function _compile_section_start($tag_args)
    {
        $attrs = $this->_parse_attrs($tag_args);

        $output = "<?php ";
        $section_name = $attrs['name'];
        if (empty($section_name)) {
            $this->_syntax_error("missing section name");
        }

        $output .= "unset(\$_sections[$section_name]);\n";
        $section_props = "\$_sections[$section_name]['properties']";

        foreach ($attrs as $attr_name => $attr_value) {
            switch ($attr_name) {
                case 'loop':
                    $output .= "{$section_props}['loop'] = is_array($attr_value) ? count($attr_value) : $attr_value;\n";
                    break;

                case 'show':
                    if (is_bool($attr_value))
                        $attr_value = $attr_value ? 'true' : 'false';
                    $output .= "{$section_props}['$attr_name'] = $attr_value;\n";
                    break;

                default:
                    $output .= "{$section_props}['$attr_name'] = $attr_value;\n";
                    break;
            }
        }

        if (isset($attrs['loop'])) {
            $loop_check_code = "{$section_props}['loop'] > 0 && ";
        } else {
            $output .= "{$section_props}['loop'] = 1;\n";
        }

        if (isset($attrs['show'])) {
            $show_check_code = "{$section_props}['show'] && ";
        } else {
            $output .= "{$section_props}['show'] = {$section_props}['loop'] > 0;\n";
        }

        $output .= "if ($loop_check_code $show_check_code true): ";

        $output .= "
            for ({$section_props}['index'] = 0;
                 {$section_props}['index'] < {$section_props}['loop'];
                 {$section_props}['index']++):\n";
        $output .= "{$section_props}['rownum'] = {$section_props}['index'] + 1;\n";
        $output .= "{$section_props}['index_prev'] = {$section_props}['index'] - 1;\n";
        $output .= "{$section_props}['index_next'] = {$section_props}['index'] + 1;\n";
	    $output .= "{$section_props}['first']      = ({$section_props}['index'] == 0);\n";
	    $output .= "{$section_props}['last']       = ({$section_props}['index'] == {$section_props}['loop']-1);\n";

        $output .= "?>";

        return $output;
    }

/*======================================================================*\
    Function: _compile_if_tag
    Purpose:  Compile {if ...} tag
\*======================================================================*/
    function _compile_if_tag($tag_args, $elseif = false)
    {
        /* Tokenize args for 'if' tag. */
        preg_match_all('/(?:
                         "[^"\\\\]*(?:\\\\.[^"\\\\]*)*"         | # match all double quoted strings allowed escaped double quotes
                         \'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'     | # match all single quoted strings allowed escaped single quotes
                         [()]                                   | # match parentheses
                         [^"\'\s()]+                              # match any other token that is not any of the above
                        )/x', $tag_args, $match);
        $tokens = $match[0];

        $this->_parse_vars_props($tokens);

        $is_arg_stack = array();

        for ($i = 0; $i < count($tokens); $i++) {
            $token = &$tokens[$i];
            switch ($token) {
                case 'eq':
                    $token = '==';
                    break;

                case 'ne':
                case 'neq':
                    $token = '!=';
                    break;

                case 'lt':
                    $token = '<';
                    break;

                case 'le':
                case 'lte':
                    $token = '<=';
                    break;

                case 'gt':
                    $token = '>';
                    break;

                case 'ge':
                case 'gte':
                    $token = '>=';
                    break;

                case 'and':
                    $token = '&&';
                    break;

                case 'or':
                    $token = '||';
                    break;

                case 'not':
                    $token = '!';
                    break;

                case 'mod':
                    $token = '%';
                    break;

                case '(':
                    array_push($is_arg_stack, $i);
                    break;

                case 'is':
                    /* If last token was a ')', we operate on the parenthesized
                       expression. The start of the expression is on the stack.
                       Otherwise, we operate on the last encountered token. */
                    if ($tokens[$i-1] == ')')
                        $is_arg_start = array_pop($is_arg_stack);
                    else
                        $is_arg_start = $i-1;
                    /* Construct the argument for 'is' expression, so it knows
                       what to operate on. */
                    $is_arg = implode(' ', array_slice($tokens, $is_arg_start, $i - $is_arg_start));

                    /* Pass all tokens from next one until the end to the
                       'is' expression parsing function. The function will
                       return modified tokens, where the first one is the result
                       of the 'is' expression and the rest are the tokens it
                       didn't touch. */
                    $new_tokens = $this->_parse_is_expr($is_arg, array_slice($tokens, $i+1));

                    /* Replace the old tokens with the new ones. */
                    array_splice($tokens, $is_arg_start, count($tokens), $new_tokens);

                    /* Adjust argument start so that it won't change from the
                       current position for the next iteration. */
                    $i = $is_arg_start;
                    break;
            }
        }

        if ($elseif)
            return '<?php elseif ('.implode(' ', $tokens).'): ?>';
        else
            return '<?php if ('.implode(' ', $tokens).'): ?>';
    }

/*======================================================================*\
    Function: _parse_is_expr
    Purpose:  Parse is expression
\*======================================================================*/
    function _parse_is_expr($is_arg, $tokens)
    {
        $expr_end = 0;

        if (($first_token = array_shift($tokens)) == 'not') {
            $negate_expr = true;
            $expr_type = array_shift($tokens);
        } else
            $expr_type = $first_token;

        switch ($expr_type) {
            case 'even':
                if ($tokens[$expr_end] == 'by') {
                    $expr_end++;
                    $expr_arg = $tokens[$expr_end++];
                    $expr = "!(($is_arg / $expr_arg) % $expr_arg)";
                }
                else
                    $expr = "!($is_arg % 2)";
                break;

            case 'odd':
                if ($tokens[$expr_end] == 'by') {
                    $expr_end++;
                    $expr_arg = $tokens[$expr_end++];
                    $expr = "(($is_arg / $expr_arg) % $expr_arg)";
                }
                else
                    $expr = "($is_arg % 2)";
                break;

            case 'div':
                if ($tokens[$expr_end] == 'by') {
                    $expr_end++;
                    $expr_arg = $tokens[$expr_end++];
                    $expr = "!($is_arg % $expr_arg)";
                } else {
                    $this->_syntax_error("expecting 'by' after 'div'");
                }
                break;

            default:
                $this->_syntax_error("unknown 'is' expression - '$expr_type'");
                break;
        }

        if ($negate_expr) {
            $expr = "!($expr)";
        }

        array_splice($tokens, 0, $expr_end, $expr);

        return $tokens;
    }

/*======================================================================*\
    Function: _parse_attrs
    Purpose:  Parse attribute string
\*======================================================================*/
    function _parse_attrs($tag_args, $quote = true)
    {
        /* Tokenize tag attributes. */
        preg_match_all('/(?:"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"       | 
                          \'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'     | (?>[^"\'=\s]+)
                         )+ |
                         [=]
                        /x', $tag_args, $match);
        $tokens       = $match[0];
        $var_delims   = array('$', '#', '%');

        $attrs = array();
        /* Parse state:
            0 - expecting attribute name
            1 - expecting '='
            2 - expecting attribute value (not '=') */
        $state = 0;

        foreach ($tokens as $token) {
            switch ($state) {
                case 0:
                    /* If the token is a valid identifier, we set attribute name
                       and go to state 1. */
                    if (preg_match('!^\w+$!', $token)) {
                        $attr_name = $token;
                        $state = 1;
                    } else
                        $this->_syntax_error("invalid attribute name - '$token'");
                    break;

                case 1:
                    /* If the token is '=', then we go to state 2. */
                    if ($token == '=') {
                        $state = 2;
                    } else
                        $this->_syntax_error("expecting '=' after attribute name");
                    break;

                case 2:
                    /* If token is not '=', we set the attribute value and go to
                       state 0. */
                    if ($token != '=') {
                        /* We booleanize the token if it's a non-quoted possible
                           boolean value. */
                        if (preg_match('!^(on|yes|true)$!', $token))
                            $token = true;
                        else if (preg_match('!^(off|no|false)$!', $token))
                            $token = false;
                        /* If the token is not variable (doesn't start with
                           '$', '#', or '%') and not enclosed in single or
                           double quotes we single-quote it. */
                        else if ($quote && !in_array($token{0}, $var_delims) &&
                                 !(($token{0} == '"' || $token[0] == "'") &&
                                 $token{strlen($token)-1} == $token{0}))
                            $token = "'".$token."'";

                        $attrs[$attr_name] = $token;
                        $state = 0;
                    } else
                        $this->_syntax_error("'=' cannot be an attribute value");
                    break;
            }
        }

        $this->_parse_vars_props($attrs);

        return $attrs;
    }

/*======================================================================*\
    Function: _preg_grep
    Purpose:  Emulate PHP's preg_grep()
\*======================================================================*/
    function _preg_grep($pattern, $array)
    {
        $result = array();

        foreach ($array as $key => $entry) {
            if (preg_match($pattern, $entry))
                $result[$key] = $entry;
        }

        return $result;
    }

/*======================================================================*\
    Function: _parse_vars_props
    Purpose:
\*======================================================================*/
    function _parse_vars_props(&$tokens)
    {        
        $qstr_regexp = '"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"|\'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'';

        /* preg_grep() was fixed to return keys properly in 4.0.4 and later. To
           allow people to use older versions of PHP we emulate preg_grep() and
           use the version check to see what function to call. */
        if (strnatcmp(PHP_VERSION, '4.0.4') >= 0) {
            $var_exprs = preg_grep('!^\$(\w+(\.\w+)?/)*\w+(?>\.\w+)*(?>\|@?\w+(:(?>' .  $qstr_regexp . '|[^|]+))*)*$!', $tokens);
            $conf_var_exprs = preg_grep('!^#(\w+)#(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|]+))*)*$!', $tokens);
            $sect_prop_exprs = preg_grep('!^%\w+\.\w+%(?>\|@?\w+(:(?>' .  $qstr_regexp .  '|[^|]+))*)*$!', $tokens);
        } else {
            $var_exprs = $this->_preg_grep('!^\$(\w+(\.\w+)?/)*\w+(?>\.\w+)*(?>\|@?\w+(:(?>' .  $qstr_regexp .  '|[^|]+))*)*$!', $tokens);
            $conf_var_exprs = $this->_preg_grep('!^#(\w+)#(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|]+))*)*$!', $tokens);
            $sect_prop_exprs = $this->_preg_grep('!^%\w+\.\w+%(?>\|@?\w+(:(?>' .  $qstr_regexp .  '|[^|]+))*)*$!', $tokens);
        }

        if (count($var_exprs)) {
            foreach ($var_exprs as $expr_index => $var_expr) {
                $tokens[$expr_index] = $this->_parse_var($var_expr);
            }
        }

        if (count($conf_var_exprs)) {
            foreach ($conf_var_exprs as $expr_index => $var_expr) {
                $tokens[$expr_index] = $this->_parse_conf_var($var_expr);
            }
        }

        if (count($sect_prop_exprs)) {
            foreach ($sect_prop_exprs as $expr_index => $section_prop_expr) {
                $tokens[$expr_index] = $this->_parse_section_prop($section_prop_expr);
            }
        }
    }

/*======================================================================*\
    Function: _parse_var
    Purpose:
\*======================================================================*/
    function _parse_var($var_expr)
    {
        list($var_ref, $modifiers) = explode('|', substr($var_expr, 1), 2);

        $sections = explode('/', $var_ref);
        $props = explode('.', array_pop($sections));
        $var_name = array_shift($props);

        $output = "\$$var_name";

        foreach ($sections as $section_ref) {
            list($section, $section_prop) = explode('.', $section_ref);
            if (!isset($section_prop))
                $section_prop = 'index';
            $output .= "[\$_sections['$section']['properties']['$section_prop']]";
        }
        foreach ($props as $prop) {
            $output .= "['$prop']";
        }

        $this->_parse_modifiers($output, $modifiers);

        return $output;
    }

/*======================================================================*\
    Function: _parse_conf_var
    Purpose:
\*======================================================================*/
    function _parse_conf_var($conf_var_expr)
    {
        list($var_ref, $modifiers) = explode('|', $conf_var_expr, 2);

        $var_name = substr($var_ref, 1, -1);

        $output = "\$_config['$var_name']";

        $this->_parse_modifiers($output, $modifiers);

        return $output;
    }

/*======================================================================*\
    Function: _parse_section_prop
    Purpose:
\*======================================================================*/
    function _parse_section_prop($section_prop_expr)
    {
        list($var_ref, $modifiers) = explode('|', $section_prop_expr, 2);

        preg_match('!%(\w+)\.(\w+)%!', $var_ref, $match);
        $section_name = $match[1];
        $prop_name = $match[2];

        $output = "\$_sections['$section_name']['properties']['$prop_name']";

        $this->_parse_modifiers($output, $modifiers);

        return $output;
    }

/*======================================================================*\
    Function: _parse_modifiers
    Purpose:
\*======================================================================*/
    function _parse_modifiers(&$output, $modifier_string)
    {
        $qstr_regexp = '"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"|\'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'';
        preg_match_all('!\|(@?\w+)((?>:(?:'. $qstr_regexp . '|[^|]+))*)!', '|' . $modifier_string, $match);
        list(, $modifiers, $modifier_arg_strings) = $match;

        for ($i = 0; $i < count($modifiers); $i++) {
            $modifier_name = $modifiers[$i];
            preg_match_all('!:(' . $qstr_regexp . '|[^:]+)!', $modifier_arg_strings[$i], $match);
            $modifier_args = $match[1];

            if ($modifier_name{0} == '@') {
                $map_array = 'false';
                $modifier_name = substr($modifier_name, 1);
            } else
                $map_array = 'true';

            /*
             * First we lookup the modifier function name in the registered
             * modifiers table.
             */
            $mod_func_name = $this->custom_mods[$modifier_name];

            /*
             * If we don't find that modifier there, we assume it's just a PHP
             * function name.
             */
            if (!isset($mod_func_name))
                $mod_func_name = $modifier_name;

            if (!function_exists($mod_func_name)) {
                $this->_syntax_error("modifier '$modifier_name' is not implemented", E_USER_WARNING);
                continue;
            }

            $this->_parse_vars_props($modifier_args);

            if (count($modifier_args) > 0)
                $modifier_args = ', '.implode(', ', $modifier_args);
            else
                $modifier_args = '';

            $output = "_smarty_mod_handler('$mod_func_name', $map_array, $output$modifier_args)";
        }
    }

}

?>
