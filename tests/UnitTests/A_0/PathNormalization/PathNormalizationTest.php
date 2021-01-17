<?php
/*
 * This file is part of the Smarty PHPUnit tests.
 */

/**
 * class for path normalization tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PathNormalizationTest extends PHPUnit_Smarty
{
    /*
     * Setup test fixture
     */
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testNormalizeToAbsolute() {
        $d = $this->smarty->_realpath('./foo/a.foo', true);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR . 'a.foo', $d);
    }
    /**public function testNormalizeAbsolute() {
        if (DIRECTORY_SEPARATOR == '/') {
            $d = $this->smarty->_realpath('\\foo\\a.foo', true);
            $this->assertEquals('/foo/a.foo', $d);
        } else {
            $d = $this->smarty->_realpath('/foo/a.foo', true);
            $this->assertEquals(substr(getcwd(), 0, 2) . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR . 'a.foo', $d);
            $d = $this->smarty->_realpath(substr(getcwd(), 0, 2) . '/foo/a.foo', true);
            $this->assertEquals(substr(getcwd(), 0, 2) . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR . 'a.foo', $d);
        }
    }
 * */
    public function testNormalizeToAbsoluteNoStart() {
        $d = $this->smarty->_realpath('foo/a.foo', true);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR . 'a.foo', $d);
    }
    public function testNormalizeToAbsoluteNoStart2() {
        $d = $this->smarty->_realpath('a.foo', true);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'a.foo', $d);
    }
    public function testNormalizeToAbsolutePathLeadingDot() {
        $d = $this->smarty->_realpath('.foo/a.foo', true);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . '.foo' . DIRECTORY_SEPARATOR . 'a.foo', $d);
    }
    public function testNormalizeToAbsolutePathTrailingDot() {
        $d = $this->smarty->_realpath('./foo./a.foo', true);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo.' . DIRECTORY_SEPARATOR . 'a.foo', $d);
    }
    public function testNormalizeToAbsolutePathBubbleUp() {
        $ds = '\\' . DIRECTORY_SEPARATOR;
        $prefix = preg_replace("#[{$ds}][^{$ds}]+$#", '', getcwd());
        $d = $this->smarty->_realpath('./../a.foo', true);
        $this->assertEquals($prefix . DIRECTORY_SEPARATOR . 'a.foo', $d);
    }
    public function testNormalizeToAbsolutePathBubbleUp2() {
        $ds = '\\' . DIRECTORY_SEPARATOR;
        $prefix = preg_replace("#[{$ds}][^{$ds}]+$#", '', getcwd());
        $d = $this->smarty->_realpath('././../././a.foo', true);
        $this->assertEquals($prefix . DIRECTORY_SEPARATOR . 'a.foo', $d);
    }
    public function testNormalizeToAbsolutePathBubbleUp3() {
        $ds = '\\' . DIRECTORY_SEPARATOR;
        $prefix = preg_replace("#[{$ds}][^{$ds}]+[{$ds}][^{$ds}]+$#", '', getcwd());
        $d = $this->smarty->_realpath('././.././.././a.foo', true);
        $this->assertEquals($prefix . DIRECTORY_SEPARATOR . 'a.foo', $d);
    }
    public function testNormalizeToAbsoluteKomplex() {
        $d = $this->smarty->_realpath('./foo/\\./bar/jo/wie/so/../..///.././././../aa/bb/cc/../../go/a.foo', true);
        $this->assertEquals(getcwd() . DIRECTORY_SEPARATOR . 'foo' . DIRECTORY_SEPARATOR . 'aa'. DIRECTORY_SEPARATOR . 'go' . DIRECTORY_SEPARATOR .'a.foo', $d);
    }

}
