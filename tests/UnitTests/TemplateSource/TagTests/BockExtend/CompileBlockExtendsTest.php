<?php
/**
 * Smarty PHPunit tests for Block Extends
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for block extends compiler tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileBlockExtendsTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }


    public function compiledPrefilter($text, Smarty_Internal_Template $tpl)
    {
        return str_replace('#', $tpl->getTemplateVars('test'), $text);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test block default outout
     */
    public function testBlockDefault_000_1()
    {
        $result = $this->smarty->fetch('string:{block name=test}-- block default --{/block}');
        $this->assertEquals('-- block default --', $result);
    }

    public function testBlockDefault_000_2()
    {
        $this->smarty->assign('foo', 'another');
        $result = $this->smarty->fetch('string:{block name=test}-- {$foo} block default --{/block}');
        $this->assertEquals('-- another block default --', $result);
    }

    /**
     * test just call of  parent template, no blocks predefined
     */
    public function testCompileBlockParent_001()
    {
        $result = $this->smarty->fetch('001_parent.tpl');
        $this->assertContains('Default Title', $result);
    }

    /**
     * test  child/parent template chain
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChild_002($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('002_child.tpl');
        $this->assertContains('Page Title', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with prepend
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildPrepend_003($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('003_child_prepend.tpl');
        $this->assertContains("prepend - Default Title", $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with apppend
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildAppend_004($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('004_child_append.tpl');
        $this->assertContains("Default Title - append", $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with apppend and shorttags
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildAppendShortag_005($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('005_child_append_shorttag.tpl');
        $this->assertContains("Default Title - append", $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with {$this->smarty.block.child)
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildSmartyChild_006($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('006_child_smartychild.tpl');
        $this->assertContains('here is >child text< included', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with {$this->smarty.block.parent)
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildSmartyParent_007($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('007_child_smartyparent.tpl');
        $this->assertContains('parent block Default Title is here', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain loading plugin
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildPlugin_008($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('008_child_plugin.tpl');
        $this->assertContains('escaped &lt;text&gt; 1', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }


    /**
     * test parent template with nested blocks
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockParentNested_009($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('009_parent_nested.tpl');
        $this->assertContains('Title with -default- here', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with nested block
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildNested_010($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('010_child_nested.tpl');
        $this->assertContains('Title with -content from child- here', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with nested block and include
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildNestedInclude_011($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('011_grandchild_nested_include.tpl');
        $this->assertContains('hello world', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent template chain
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockGrandChild_012($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('012_grandchild.tpl');
        $this->assertContains('Grandchild Page Title', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent template chain prepend
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockGrandChildPrepend_013($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('013_grandchild_prepend.tpl');
        $this->assertContains('grandchild prepend - child', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }
    /**
     * test  grandchild/child/parent template chain prepend
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockGrandChildPrepend_013_2($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('013_2_grandchild_prepend.tpl');
        $this->assertContains('grandchild prepend - child prepend - parent', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent template chain with {$this->smarty.block.child}
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockGrandChildSmartyChild_014($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('014_grandchild_smartychild.tpl');
        $this->assertContains('child title with - grandchild content - here', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent template chain append
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockGrandChildAppend_015($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('015_grandchild_append.tpl');
        $this->assertContains('Page Title - grandchild append', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent template chain with nested block
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockGrandChildNested_016($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('016_grandchild_nested.tpl');
        $this->assertContains('child title with -grandchild content- here', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent template chain with nested {$this->smarty.block.child}
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockGrandChildNested_017($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('017_grandchild_nested.tpl');
        $this->assertContains('child pre -grandchild content- child post', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  nested child block with hide
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildNestedHide_018($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('018_child_nested_hide.tpl');
        $this->assertContains('nested block', $result);
        $this->assertNotContains('should be hidden', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  nested child block with hide and auto_literal = false
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildNestedHideAutoLiteralFalse_019($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $this->smarty->setAutoLiteral(false);
        $result = $this->smarty->fetch('019_child_nested_hide_autoliteral.tpl');
        $this->assertContains('nested block', $result, $testName . ' - content');
        $this->assertNotContains('should be hidden', $result, $testName . ' - content2');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain starting in subtempates
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockStartSubTemplates_020($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('020_include_root.tpl');
        $this->assertContains('page 1', $result, $testName . ' - content1');
        $this->assertContains('page 2', $result, $testName . ' - content2');
        $this->assertContains('page 3', $result, $testName . ' - content3');
        $this->assertContains('block 1', $result, $testName . ' - content4');
        $this->assertContains('block 2', $result, $testName . ' - content5');
        $this->assertContains('block 3', $result, $testName . ' - content6');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent template chain with nested {$this->smarty.block.child} and {include nocache}
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockGrandChildNested_030($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('030_grandchild_nested.tpl');
        $this->assertContains('child pre -grandchild content- child post', $result, $testName . ' - content');
        $this->assertContains('include:' . $testNumber, $result, $testName . ' - content 2');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent dependency test1
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testCompileBlockGrandChildMustCompile_021_1()
    {
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
        $this->smarty->_cache['template_objects'] = null;
        $tpl2 = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test2
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_2()
    {
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_grandchild.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
        $this->smarty->_cache['template_objects'] = null;
        $tpl2 = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test3
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_3()
    {
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_child.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
    }
    /**
     * test  grandchild/child/parent dependency test3
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_32()
    {
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl2 = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test4
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testCompileBlockGrandChildMustCompile_021_4()
    {
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_parent.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test4
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testCompileBlockGrandChildMustCompile_021_42()
    {
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl2 = $this->smarty->createTemplate('021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test dirt in child templates
     */
    public function testDirt_022()
    {
        $result = $this->smarty->fetch('022_child.tpl');
        $this->assertEquals('Page Title', $result);
    }

    /**
     * test {$this->smarty.block.child} for not existing child {block]
     */
    public function testNotExistingChildBlock_024()
    {
        $result = $this->smarty->fetch('024_parent.tpl');
        $this->assertContains('no >< child', $result);
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage tag {$smarty.block.child} used outside {block} tags
     * test {$this->smarty.block.child} outside {block]
     */
    public function testSmartyBlockChildOutsideBlock_025()
    {
         $this->smarty->fetch('025_parent.tpl');
     }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage tag {$smarty.block.parent} used outside {block} tags
     * test {$this->smarty.block.parent} outside {block]
     */
    public function testSmartyBlockParentOutsideBlock_026()
    {
        $this->smarty->fetch('026_child.tpl');
    }

    /**
     * @expectedException        SmartyException
     * @expectedExceptionMessage tag {$smarty.block.parent} used in parent template
     * test {$this->smarty.block.parent} in parent template
     */
    public function testSmartyBlockParentInParent_027()
    {
        $result = $this->smarty->fetch('027_parent.tpl');
    }


    /**
     * test  child/parent template chain
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testSmartyBlockVariablePartentInclude_28($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        $this->smarty->compile_id = 10;
        if ($merge) {
            $this->smarty->compile_id = 11;
        }
        $this->smarty->assign('foo', '028_parent_include1.tpl');
        $result = $this->smarty->fetch('028_child.tpl');
        $this->assertContains('b1-include-1--b1', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain
     * @dataProvider data
     */
    public function testSmartyBlockVariablePartentInclude_282($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $this->smarty->assign('foo', '028_parent_include2.tpl');
        $result = $this->smarty->fetch('028_child.tpl');
        $this->assertContains('b1-child-i2-include-2--b1', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }



    public function data(){
        return array(
            /*
             * caching
             * merging
             * test nr
             * result compile nr
             * result render nr
             * text
             */
            array(false, false, 1, 1, 1, 'no caching, no merge - new'),
            array(false, false, 2, 1, 2, 'no caching, no merge - exits'),
            array(true, false, 3, 3, 3, 'caching, no merge - new'),
            array(true, false, 4, 3, 3, 'caching, no merge - exits'),
            array(false, true, 5, 5, 5, 'no caching, merge - new'),
            array(false, true, 6, 5, 6, 'no caching, merge - exits'),
            array(true, true, 7, 7, 7, 'caching, merge - new'),
            array(true, true, 8, 7, 7, 'caching, merge - exits'),
        );
    }
}
