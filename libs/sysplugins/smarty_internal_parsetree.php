<?php
/**
 * Smarty Internal Plugin Templateparser Parsetrees
 * These are classes to build parsetrees in the template parser
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Thue Kristensen
 * @author     Uwe Tews
 */

/**
 * @package    Smarty
 * @subpackage Compiler
 * @ignore
 */
abstract class _smarty_parsetree
{
    /**
     * Parser object
     *
     * @var object
     */
    public $parser;
    /**
     * Buffer content
     *
     * @var mixed
     */
    public $data;

    /**
     * Subtree array
     *
     * @var array
     */
    public $subtrees = array();

    /**
     * Return buffer
     *
     * @return string buffer content
     */
    abstract public function to_smarty_php();
}

/**
 * A complete smarty tag.
 *
 * @package    Smarty
 * @subpackage Compiler
 * @ignore
 */
class _smarty_tag extends _smarty_parsetree
{
    /**
     * Saved block nesting level
     *
     * @var int
     */
    public $saved_block_nesting;

    /**
     * Create parse tree buffer for Smarty tag
     *
     * @param object $parser parser object
     * @param string $data   content
     */
    public function __construct($parser, $data)
    {
        $this->parser = $parser;
        $this->data = $data;
        $this->saved_block_nesting = $parser->block_nesting_level;
    }

    /**
     * Return buffer content
     *
     * @return string content
     */
    public function to_smarty_php()
    {
        return $this->data;
    }

    /**
     * Return complied code that loads the evaluated output of buffer content into a temporary variable
     *
     * @return string template code
     */
    public function assign_to_var()
    {
        $var = sprintf('$_tmp%d', ++Smarty_Internal_Templateparser::$prefix_number);
        $this->parser->compiler->prefix_code[] = sprintf("<?php ob_start();\n%s\n%s=ob_get_clean();?>", preg_replace(array('/^\s*<\?php\s+/','/\s*\?>\s*$/'), '', $this->data), $var);

        return $var;
    }
}

/**
 * Code fragment inside a tag.
 *
 * @package    Smarty
 * @subpackage Compiler
 * @ignore
 */
class _smarty_code extends _smarty_parsetree
{
    /**
     * Create parse tree buffer for code fragment
     *
     * @param object $parser parser object
     * @param string $data   content
     */
    public function __construct($parser, $data)
    {
        $this->parser = $parser;
        $this->data = $data;
    }

    /**
     * Return buffer content in parentheses
     *
     * @return string content
     */
    public function to_smarty_php()
    {
        return sprintf("(%s)", $this->data);
    }
}

/**
 * Double quoted string inside a tag.
 *
 * @package    Smarty
 * @subpackage Compiler
 * @ignore
 */
class _smarty_doublequoted extends _smarty_parsetree
{
    /**
     * Create parse tree buffer for double quoted string subtrees
     *
     * @param object            $parser  parser object
     * @param _smarty_parsetree $subtree parsetree buffer
     */
    public function __construct($parser, _smarty_parsetree $subtree)
    {
        $this->parser = $parser;
        $this->subtrees[] = $subtree;
        if ($subtree instanceof _smarty_tag) {
            $this->parser->block_nesting_level = count($this->parser->compiler->_tag_stack);
        }
    }

    /**
     * Append buffer to subtree
     *
     * @param _smarty_parsetree $subtree parsetree buffer
     */
    public function append_subtree(_smarty_parsetree $subtree)
    {
        $last_subtree = count($this->subtrees) - 1;
        if ($last_subtree >= 0 && $this->subtrees[$last_subtree] instanceof _smarty_tag && $this->subtrees[$last_subtree]->saved_block_nesting < $this->parser->block_nesting_level) {
            if ($subtree instanceof _smarty_code) {
                $this->subtrees[$last_subtree]->data .= '<?php echo ' . $subtree->data . ';?>';
            } elseif ($subtree instanceof _smarty_dq_content) {
                $this->subtrees[$last_subtree]->data .= '<?php echo "' . $subtree->data . '";?>';
            } else {
                $this->subtrees[$last_subtree]->data .= $subtree->data;
            }
        } else {
            $this->subtrees[] = $subtree;
        }
        if ($subtree instanceof _smarty_tag) {
            $this->parser->block_nesting_level = count($this->parser->compiler->_tag_stack);
        }
    }

    /**
     * Merge subtree buffer content together
     *
     * @return string compiled template code
     */
    public function to_smarty_php()
    {
        $code = '';
        foreach ($this->subtrees as $subtree) {
            if ($code !== "") {
                $code .= ".";
            }
            if ($subtree instanceof _smarty_tag) {
                $more_php = $subtree->assign_to_var();
            } else {
                $more_php = $subtree->to_smarty_php();
            }

            $code .= $more_php;

            if (!$subtree instanceof _smarty_dq_content) {
                $this->parser->compiler->has_variable_string = true;
            }
        }

        return $code;
    }
}

/**
 * Raw chars as part of a double quoted string.
 *
 * @package    Smarty
 * @subpackage Compiler
 * @ignore
 */
class _smarty_dq_content extends _smarty_parsetree
{
    /**
     * Create parse tree buffer with string content
     *
     * @param object $parser parser object
     * @param string $data   string section
     */
    public function __construct($parser, $data)
    {
        $this->parser = $parser;
        $this->data = $data;
    }

    /**
     * Return content as double quoted string
     *
     * @return string doubled quoted string
     */
    public function to_smarty_php()
    {
        return '"' . $this->data . '"';
    }
}

/**
 * Template element
 *
 * @package    Smarty
 * @subpackage Compiler
 * @ignore
 */
class _smarty_template_buffer extends _smarty_parsetree
{
    /**
     * Array of template elements
     *
     * @var array
     */
    public $subtrees = Array();

    /**
     * Create root of parse tree for template elements
     *
     * @param object $parser parse object
     */
    public function __construct($parser)
    {
        $this->parser = $parser;
    }

    /**
     * Append buffer to subtree
     *
     * @param _smarty_parsetree $subtree
     */
    public function append_subtree(_smarty_parsetree $subtree)
    {
        if ($subtree->data !== '') {
            $this->subtrees[] = $subtree;
        }
    }

    /**
     * Sanitize and merge subtree buffers together
     *
     * @return string template code content
     */
    public function to_smarty_php()
    {
        $code = '';
        for ($key = 0, $cnt = count($this->subtrees); $key < $cnt; $key ++) {
            if ($this->subtrees[$key] instanceof _smarty_text) {
                $subtree = $this->subtrees[$key]->to_smarty_php();
                while ($key + 1 < $cnt && ($this->subtrees[$key+1] instanceof _smarty_text || $this->subtrees[$key +1]->data == '')) {
                    $key++;
                    if ($this->subtrees[$key]->data == '') {
                        continue;
                    }
                    $subtree .= $this->subtrees[$key]->to_smarty_php();
                }
                if ($subtree == '') {
                    continue;
                }
                $code .= preg_replace('/(<%|%>|<\?php|<\?|\?>|<\/?script)/', "<?php echo '\$1'; ?>\n", $subtree);
                continue;
            }
            if ($this->subtrees[$key] instanceof _smarty_tag) {
                $subtree = $this->subtrees[$key]->to_smarty_php();
                while ($key + 1 < $cnt && ($this->subtrees[$key+1] instanceof _smarty_tag || $this->subtrees[$key +1]->data == '')) {
                    $key++;
                    if ($this->subtrees[$key]->data == '') {
                        continue;
                    }
                    $newCode = $this->subtrees[$key]->to_smarty_php();
                    if ((preg_match('/^\s*<\?php\s+/', $newCode) &&  preg_match('/\s*\?>\s*$/', $subtree))) {
                        $subtree = preg_replace('/\s*\?>\s*$/', "\n", $subtree);
                        $subtree .= preg_replace('/^\s*<\?php\s+/', '', $newCode);
                    } else {
                        $subtree .= $newCode;
                    }
                }
                if ($subtree == '') {
                    continue;
                }
                $code .= $subtree;
                continue;
            }
            $code .= $this->subtrees[$key]->to_smarty_php();
        }
        return $code;
    }

}

/**
 * template text
 *
 * @package    Smarty
 * @subpackage Compiler
 * @ignore
 */
class _smarty_text extends _smarty_parsetree
{
    /**
     * Create template text buffer
     *
     * @param object $parser parser object
     * @param string $data   text
     */
    public function __construct($parser, $data)
    {
        $this->parser = $parser;
        $this->data = $data;
    }

    /**
     * Return buffer content
     *
     * @return strint text
     */
    public function to_smarty_php()
    {
        return $this->data;
    }
}
