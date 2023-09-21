<?php
/*
 * This file is part of the Smarty PHPUnit tests.
 */

/**
 * class for 'escapeHtml' property tests
 *
 * 
 * 
 * 
 */
class AutoEscapeTest extends PHPUnit_Smarty
{
    /*
     * Setup test fixture
     */
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->setEscapeHtml(true);
    }

    /**
     * test 'escapeHtml' property
     */
    public function testAutoEscape()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("&lt;a@b.c&gt;", $this->smarty->fetch($tpl));
    }

    /**
     * test 'escapeHtml' property
     * @group issue906
     */
    public function testAutoEscapeDoesNotEscapeFunctionPlugins()
    {
        $this->smarty->registerPlugin(
            \Smarty\Smarty::PLUGIN_FUNCTION,
            'horizontal_rule',
            function ($params, $smarty)	{ return "<hr>"; }
        );
        $tpl = $this->smarty->createTemplate('eval:{horizontal_rule}');
        $this->assertEquals("<hr>", $this->smarty->fetch($tpl));
    }

    /**
     * test 'escapeHtml' property
     * @group issue906
     */
    public function testAutoEscapeDoesNotEscapeBlockPlugins()
    {
        $this->smarty->registerPlugin(
            \Smarty\Smarty::PLUGIN_BLOCK,
            'paragraphify',
            function ($params, $content)	{ return $content == null ? null :  "<p>".$content."</p>"; }
        );
        $tpl = $this->smarty->createTemplate('eval:{paragraphify}hi{/paragraphify}');
        $this->assertEquals("<p>hi</p>", $this->smarty->fetch($tpl));
    }

}
