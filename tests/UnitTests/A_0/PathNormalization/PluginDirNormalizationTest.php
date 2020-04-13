<?php
/*
 * This file is part of the Smarty PHPUnit tests.
 */

/**
 * class for path normalization tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class PluginNormalizationTest extends PHPUnit_Smarty
{
    /*
     * Setup test fixture
     */
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testGetPluginsDefaultDir()
    {
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(SMARTY_PLUGINS_DIR, $result[ 0 ]);
    }
    public function testGetPluginsDefaultDir2()
    {
        $this->smarty->addPluginsDir('./foo/');
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(2, count($result));
        $this->assertEquals(SMARTY_PLUGINS_DIR, $result[ 0 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 1 ]);
    }

    public function testSetPluginsDir1()
    {
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setPluginsDir('foo');
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetPluginsDir2()
    {
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setPluginsDir('foo/');
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetPluginsDir3()
    {
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setPluginsDir('foo/.');
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetPluginsDir4()
    {
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setPluginsDir('foo/./');
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetPluginsDir5()
    {
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setPluginsDir('.foo');
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . '.foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetPluginsDir6()
    {
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setPluginsDir('..foo');
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . '..foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
    }

    public function testSetPluginsDir7()
    {
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->smarty->setPluginsDir('../foo');
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(1, count($result));
        $this->assertEquals(substr(getcwd(), 0, - strlen(basename(getcwd()))) . 'foo' . DIRECTORY_SEPARATOR,
                            $result[ 0 ]);
    }

    public function testSetPluginsDir8()
    {
        $this->smarty->setPluginsDir(array('foo', 'bah'));
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(2, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'bah' . DIRECTORY_SEPARATOR, $result[ 1 ]);
    }

    public function testSetPluginsDir9()
    {
        $this->smarty->setPluginsDir(array('foo', 'bah'));
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(2, count($result));
        $this->smarty->addPluginsDir('blar');
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(3, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'bah' . DIRECTORY_SEPARATOR, $result[ 1 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'blar' . DIRECTORY_SEPARATOR, $result[ 2 ]);
    }

    public function testSetPluginsDir10()
    {
        $this->smarty->setPluginsDir(array('foo', 'bah'));
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(2, count($result));
        $this->smarty->addPluginsDir(array('blar', 'smarty'));
        $result = $this->smarty->getPluginsDir();
        $this->assertEquals(4, count($result));
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR, $result[ 0 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'bah' . DIRECTORY_SEPARATOR, $result[ 1 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'blar' . DIRECTORY_SEPARATOR, $result[ 2 ]);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'smarty' . DIRECTORY_SEPARATOR, $result[ 3 ]);
    }

}
