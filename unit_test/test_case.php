<?php

require_once './config.php';
require_once SMARTY_DIR . 'Smarty.class.php';
require_once 'PHPUnit.php';
    
class SmartyTest extends PHPUnit_TestCase {
    // contains the object handle of the string class
    var $abc;
    // constructor of the test suite
    function SmartyTest($name) {
       $this->PHPUnit_TestCase($name);
    }
    // called before the test functions will be executed    
    // this function is defined in PHPUnit_TestCase and overwritten 
    // here
    function setUp() {
        // create a new instance of String with the
        // string 'abc'
        $this->smarty = new Smarty;
    }
    // called after the test functions are executed    
    // this function is defined in PHPUnit_TestCase and overwritten 
    // here    
    function tearDown() {
        // delete your instance
        unset($this->smarty);
    }
    // test that template_dir exists
    function test_template_dir_exists() {
        $this->assertTrue(file_exists($this->smarty->template_dir));                       
    }
    // test that template_dir is a directory
    function test_template_dir_is_dir() {
        $this->assertTrue(is_dir($this->smarty->template_dir));                       
    }
    // test that template_dir is readable
    function test_template_dir_is_readable() {
        $this->assertTrue(is_readable($this->smarty->template_dir));                       
    }
    // test that config_dir exists
    function test_config_dir_exists() {
        $this->assertTrue(file_exists($this->smarty->config_dir));                       
    }
    // test that config_dir is a directory
    function test_config_dir_is_dir() {
        $this->assertTrue(is_dir($this->smarty->config_dir));                       
    }
    // test that config_dir is readable
    function test_config_dir_is_readable() {
        $this->assertTrue(is_readable($this->smarty->config_dir));                       
    }
    // test that compile_dir exists
    function test_compile_dir_exists() {
        $this->assertTrue(file_exists($this->smarty->compile_dir));                       
    }
    // test that compile_dir is a directory
    function test_compile_dir_is_dir() {
        $this->assertTrue(is_dir($this->smarty->compile_dir));                       
    }
    // test that compile_dir is readable
    function test_compile_dir_is_readable() {
        $this->assertTrue(is_readable($this->smarty->compile_dir));                       
    }
    // test that compile_dir is writable
    function test_compile_dir_is_writable() {
        $this->assertTrue(is_writable($this->smarty->compile_dir));                       
    }
    // test that cache_dir exists
    function test_cache_dir_exists() {
        $this->assertTrue(file_exists($this->smarty->cache_dir));                       
    }
    // test that cache_dir is a directory
    function test_cache_dir_is_dir() {
        $this->assertTrue(is_dir($this->smarty->cache_dir));                       
    }
    // test that cache_dir is readable
    function test_cache_dir_is_readable() {
        $this->assertTrue(is_readable($this->smarty->cache_dir));                       
    }
    // test that cache_dir is writable
    function test_cache_dir_is_writable() {
        $this->assertTrue(is_writable($this->smarty->cache_dir));                       
    }

  }
?>
