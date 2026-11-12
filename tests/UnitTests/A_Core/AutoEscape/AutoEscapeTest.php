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

    /**
     * test nofilter disables autoescape
     */
    public function testAutoEscapeNofilter() {
        $tpl = $this->smarty->createTemplate('eval:{$foo nofilter}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("<a@b.c>", $this->smarty->fetch($tpl));
    }

    /**
     * test autoescape + explicit escape still escapes when nofilter is used
     * @group issue1188
     */
    public function testAutoEscapeEscapeWithNofilter() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|escape nofilter}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("&lt;a@b.c&gt;", $this->smarty->fetch($tpl));
    }

    /**
     * test autoescape + escape followed by an HTML-producing modifier, with nofilter
     * @group issue1188
     */
    public function testAutoEscapeEscapeNl2brWithNofilter() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|escape|nl2br nofilter}');
        $tpl->assign('foo', "<a@b.c>\nsecond line");
        $this->assertEquals("&lt;a@b.c&gt;<br />\nsecond line", $this->smarty->fetch($tpl));
    }

    /**
     * test autoescape + escape followed by an HTML-producing modifier, without nofilter
     * @group issue1188
     */
    public function testAutoEscapeEscapeNl2br() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|escape|nl2br}');
        $tpl->assign('foo', "<a@b.c>\nsecond line");
        $this->assertEquals("&lt;a@b.c&gt;<br />\nsecond line", $this->smarty->fetch($tpl));
    }

    /**
     * test autoescape + escape modifier honors the double_encode parameter
     * @group issue1188
     */
    public function testAutoEscapeEscapeHonorsDoubleEncodeParameter() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|escape:\'html\':\'UTF-8\':false}');
        $tpl->assign('foo', '&amp; <a@b.c>');
        $this->assertEquals("&amp; &lt;a@b.c&gt;", $this->smarty->fetch($tpl));
    }

    /**
     * test that escape used in an assign attribute does not disable
     * auto-escaping of the next printed variable
     * @group issue1188
     */
    public function testRawOutputDoesNotLeakFromAssignAttribute() {
        $tpl = $this->smarty->createTemplate('eval:{$foo|escape assign=bar}{$foo}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("&lt;a@b.c&gt;", $this->smarty->fetch($tpl));
    }

    /**
     * test that escape used in an {assign} tag does not disable
     * auto-escaping of the next printed variable
     * @group issue1188
     */
    public function testRawOutputDoesNotLeakFromAssignTag() {
        $tpl = $this->smarty->createTemplate('eval:{assign var=bar value=$foo|escape}{$foo}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("&lt;a@b.c&gt;", $this->smarty->fetch($tpl));
    }

    /**
     * test that an escaping modifier used in an {if} condition does not disable
     * auto-escaping of the next printed variable
     * @group issue1188
     */
    public function testRawOutputDoesNotLeakFromIfCondition() {
        $tpl = $this->smarty->createTemplate('eval:{if $foo|escape:\'url\'}{/if}{$foo}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("&lt;a@b.c&gt;", $this->smarty->fetch($tpl));
    }

    /**
     * test that the raw modifier used in an {assign} tag does not disable
     * auto-escaping of the next printed variable
     * @group issue1188
     */
    public function testRawOutputDoesNotLeakFromRawInAssignTag() {
        $tpl = $this->smarty->createTemplate('eval:{assign var=bar value=$foo|raw}{$foo}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("&lt;a@b.c&gt;", $this->smarty->fetch($tpl));
    }

}
