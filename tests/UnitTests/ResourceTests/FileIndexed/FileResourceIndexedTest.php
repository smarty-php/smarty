<?php

/**
 * Smarty PHPunit tests for File resources
 *
 * @package                PHPunit
 * @author                 Rodney Rehm
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class FileResourceIndexedTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addTemplateDir(dirname(__FILE__) . '/templates_2');
        // note that 10 is a string!
        $this->smarty->addTemplateDir(dirname(__FILE__) . '/templates_3', '10');
        $this->smarty->addTemplateDir(dirname(__FILE__) . '/templates_4', 'foo');
     }

    public function testInit()
    {
        $this->cleanDirs();
    }

    public function testGetTemplateFilepath()
    {
        $tpl = $this->smarty->createTemplate('dirname.tpl');
        $this->assertEquals($this->normalizePath("./templates/dirname.tpl"), $tpl->source->filepath);
    }

    public function testGetTemplateFilepathNumber()
    {
        $tpl = $this->smarty->createTemplate('[1]dirname.tpl');
        $this->assertEquals($this->normalizePath('./templates_2/dirname.tpl'), $tpl->source->filepath);
    }

    public function testGetTemplateFilepathNumeric()
    {
        $tpl = $this->smarty->createTemplate('[10]dirname.tpl');
        $this->assertEquals($this->normalizePath('./templates_3/dirname.tpl'), $tpl->source->filepath);
    }

    public function testGetTemplateFilepathName()
    {
        $tpl = $this->smarty->createTemplate('[foo]dirname.tpl');
        $this->assertEquals($this->normalizePath('./templates_4/dirname.tpl'), $tpl->source->filepath);
    }

    public function testFetch()
    {
        $tpl = $this->smarty->createTemplate('dirname.tpl');
        $this->assertEquals('templates', $this->smarty->fetch($tpl));
    }

    public function testFetchNumber()
    {
        $tpl = $this->smarty->createTemplate('[1]dirname.tpl');
        $this->assertEquals('templates_2', $this->smarty->fetch($tpl));
    }

    public function testFetchNumeric()
    {
        $tpl = $this->smarty->createTemplate('[10]dirname.tpl');
        $this->assertEquals('templates_3', $this->smarty->fetch($tpl));
    }
    public function testFetchNumeric2()
    {
        $tpl = $this->smarty->createTemplate('[10, 1]dirname10.tpl');
        $this->assertEquals('templates_3', $this->smarty->fetch($tpl));
    }
    public function testFetchNumeric3()
    {
        $tpl = $this->smarty->createTemplate('[10, 1]dirname1.tpl');
        $this->assertEquals('templates_2', $this->smarty->fetch($tpl));
    }

    public function testFetchName()
    {
        $tpl = $this->smarty->createTemplate('[foo]dirname.tpl');
        $this->assertEquals('templates_4', $this->smarty->fetch($tpl));
    }
    public function testFetchName1()
    {
        $tpl = $this->smarty->createTemplate('[10,0,1,foo]dirname_foo.tpl');
        $this->assertEquals('dirname_foo', $this->smarty->fetch($tpl));
    }
    public function testFetchName2()
    {
        $tpl = $this->smarty->createTemplate('[0,1,foo,10]dirname_x.tpl');
        $this->assertEquals('templates_2', $this->smarty->fetch($tpl));
    }
    public function testFetchName3()
    {
        $tpl = $this->smarty->createTemplate('[0,10,foo,1]dirname_x.tpl');
        $this->assertEquals('templates_4', $this->smarty->fetch($tpl));
    }

    public function testGetCompiledFilepath()
    {
        $tpl = $this->smarty->createTemplate('[foo]dirname.tpl');
        $this->assertEquals($this->buildCompiledPath($tpl, false, false, null, 'dirname.tpl', 'file', $this->smarty->getTemplateDir('foo')), $tpl->compiled->filepath);
    }

    public function testGetCachedFilepath()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('[foo]dirname.tpl');
        $this->assertEquals($this->buildCachedPath($tpl, false, null, null, 'dirname.tpl', 'file', $this->smarty->getTemplateDir('foo'))
            , $tpl->cached->filepath);
    }
}
