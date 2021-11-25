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
    modifier can_buy_scales(uint usd_invested){
        require ((usd_invested * usd_to_scales) + total_scales_bought <= max_scales);
        _; // <-- this is to ensure that the function only will be applied if the condition is true
    }

    // Functions Implementation

    // Return the value in scales
    function equity_in_scales(address investor) external view returns(uint){
        return equity_scales[investor];
    }

    // Return the value in dollar
    function equity_in_usd(address investor) external view returns(uint){
        return equity_usd[investor];
    }

    // Buy
    function buy_scales(address investor, uint usd_invested) external
    can_buy_scales(usd_invested) {
        // Amount of Scales
        uint scales_bought = usd_invested * usd_to_scales;
        // Updating the value in scales
        equity_scales[investor] += scales_bought;
        // Updating the value in dollar
        equity_usd[investor] = equity_scales[investor] / usd_to_scales;

        total_scales_bought += scales_bought;
    }

    // Sell
    function sell_scales(address investor, uint scales_sold) external {
        // Updating the value in scales
        equity_scales[investor] -= scales_sold;
        // Updating the value in dollar
        equity_usd[investor] -= scales_sold / usd_to_scales;

        total_scales_bought -= scales_sold;
    }
}
