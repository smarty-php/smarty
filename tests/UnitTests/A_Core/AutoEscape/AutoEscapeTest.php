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

    /**
     * test autoescape + raw modifier
     */
    public function testAutoEscapeRaw() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|raw}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("<a@b.c>", $this->smarty->fetch($tpl));
    }

    /**
     * test autoescape + escape modifier = no double-escaping
     */
    public function testAutoEscapeNoDoubleEscape() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|escape}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("&lt;a@b.c&gt;", $this->smarty->fetch($tpl));
    }

    /**
     * test autoescape + escape modifier = force double-escaping
     */
    public function testAutoEscapeForceDoubleEscape() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|escape:\'force\'}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("&amp;lt;a@b.c&amp;gt;", $this->smarty->fetch($tpl));
    }

    /**
     * test autoescape + escape modifier = special escape
     */
    public function testAutoEscapeSpecialEscape() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|escape:\'url\'}');
        $tpl->assign('foo', 'aa bb');
        $this->assertEquals("aa%20bb", $this->smarty->fetch($tpl));
    }

    /**
     * test autoescape + escape modifier = special escape
     */
    public function testAutoEscapeSpecialEscape2() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|escape:\'url\'}');
        $tpl->assign('foo', '<BR>');
        $this->assertEquals("%3CBR%3E", $this->smarty->fetch($tpl));
    }

    /**
     * test autoescape + escape modifier = special escape
     */
    public function testAutoEscapeSpecialEscape3() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|escape:\'htmlall\'}');
        $tpl->assign('foo', '<BR>');
        $this->assertEquals("&lt;BR&gt;", $this->smarty->fetch($tpl));
    }


    /**
     * test autoescape + escape modifier = special escape
     */
    public function testAutoEscapeSpecialEscape4() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|escape:\'javascript\'}');
        $tpl->assign('foo', '<\'');
        $this->assertEquals("<\\'", $this->smarty->fetch($tpl));
    }

}
