<?php

abstract class _smarty_parsetree {
  abstract public function to_smarty_php();
}

/* A complete smarty tag. */

class _smarty_tag extends _smarty_parsetree {
    public $parser;
    public $data;
    public $saved_block_nesting;
    function __construct($parser, $data)
    {
        $this->parser = $parser;
        $this->data = $data;
        $this->saved_block_nesting = $parser->block_nesting_level;
    } 

    public function to_smarty_php()
    {
        return $this->data;
    } 

    public function assign_to_var()
    {
        $var = sprintf('$_tmp%d', ++$this->parser->prefix_number);
        $this->parser->compiler->prefix_code[] = sprintf('<?php ob_start();?>%s<?php %s=ob_get_clean();?>',
            $this->data, $var);
        return $var;
    } 
} 

/* Code fragment inside a tag. */
class _smarty_code extends _smarty_parsetree {
    public $parser;
    public $data;
    function __construct($parser, $data)
    {
        $this->parser = $parser;
        $this->data = $data;
    } 

    public function to_smarty_php()
    {
        return sprintf("(%s)", $this->data);
    } 
} 

/* Double quoted string inside a tag. */
class _smarty_doublequoted extends _smarty_parsetree {
    public $parser;
    public $subtrees = Array();
    function __construct($parser, _smarty_parsetree $subtree)
    {
        $this->parser = $parser;
        $this->subtrees[] = $subtree;
        if ($subtree instanceof _smarty_tag) {
            $this->parser->block_nesting_level = count($this->parser->compiler->_tag_stack);
        } 
    } 

    function append_subtree(_smarty_parsetree $subtree)
    {
        $last_subtree = count($this->subtrees)-1;
        if ($last_subtree >= 0 && $this->subtrees[$last_subtree] instanceof _smarty_tag && $this->subtrees[$last_subtree]->saved_block_nesting < $this->parser->block_nesting_level) {
            if ($subtree instanceof _smarty_code) {
                $this->subtrees[$last_subtree]->data .= '<?php echo ' . $subtree->data . ';?>';
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

//        $code = sprintf("(%s)", $code);
        return $code;
    } 
} 

/* Raw chars as part of a double quoted string. */
class _smarty_dq_content extends _smarty_parsetree {
    public $data;
    function __construct($parser, $data)
    {
        $this->parser = $parser;
        $this->data = $data;
    } 

    public function to_smarty_php()
    {
        return '"' . $this->data . '"';
    } 
} 


?>