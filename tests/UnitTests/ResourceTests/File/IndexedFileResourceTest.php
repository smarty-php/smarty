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
        $path = str_replace(array("\\", "/"), DS, dirname(__FILE__));
        $this->smarty->addTemplateDir($path . DS. 'templates_2');
        // note that 10 is a string!
        $this->smarty->addTemplateDir($path . DS. 'templates_3', '10');
        $this->smarty->addTemplateDir($path . DS . 'templates_4', 'foo');
    }

    protected function relative($path)
    {
        $path = str_replace(str_replace("\\", "/", dirname(__FILE__)), '.', str_replace("\\", "/", $path));

        return $path;
    }

    public function testGetTemplateFilepath()
    {
        $tpl = $this->smarty->createTemplate('dirname.tpl');
        $this->assertEquals("./templates/dirname.tpl", $this->relative($tpl->source->filepath));
    }

    public function testGetTemplateFilepathNumber()
    {
        $tpl = $this->smarty->createTemplate('[1]dirname.tpl');
        $this->assertEquals('./templates_2/dirname.tpl', $this->relative($tpl->source->filepath));
    }

    public function testGetTemplateFilepathNumeric()
    {
        $tpl = $this->smarty->createTemplate('[10]dirname.tpl');
        $this->assertEquals('./templates_3/dirname.tpl', $this->relative($tpl->source->filepath));
    }

    public function testGetTemplateFilepathName()
    {
        $tpl = $this->smarty->createTemplate('[foo]dirname.tpl');
        $this->assertEquals('./templates_4/dirname.tpl', $this->relative($tpl->source->filepath));
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
