<?php

/**
 * Smarty PHPunit tests for File resources
 *
 * @package                PHPunit
 * @author                 Rodney Rehm
 * @backupStaticAttributes enabled
 */
class IndexedFileResourceTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->addTemplateDir(__DIR__ . '/templates_2');
        // note that 10 is a string!
        $this->smarty->addTemplateDir(__DIR__ . '/templates_3', '10');
        $this->smarty->addTemplateDir(__DIR__ . '/templates_4', 'foo');
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

    public function testFetchName()
    {
        $tpl = $this->smarty->createTemplate('[foo]dirname.tpl');
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
