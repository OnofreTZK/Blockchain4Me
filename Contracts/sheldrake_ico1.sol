// ICO Scales(SCL)
// SPDX-License-Identifier: MIT
// compiler version must be greater than or equal to 0.8.3 and less than 0.9.0
// Version
pragma solidity ^0.8.3;

contract scales_ico {
    // Number of scales_coins
    uint public max_scales = 1000000;

    // Fee based on dollar
    uint public usd_to_scales = 1;

    // Scales bought by investors
    uint public total_scales_bought = 0;

    // Equivalence functions
    mapping(address => uint) equity_scales;
    mapping(address => uint) equity_usd;

    // Verify that availability to buy scales
    modifier con_buy_scales(uint usd_invested){
        require ((usd_invested * usd_to_scales) + total_scales_bought <= max_scales);
        _; // <-- this is to ensure that the function only will be applied if the condition is true
    }

}
