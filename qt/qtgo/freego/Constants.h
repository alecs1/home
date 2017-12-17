#pragma once

#include <boost/bimap.hpp>
#include <boost/assign.hpp>

enum PlayerType:uint8_t {
    AI = 0,
    LocalHuman,
    Network,
    None
};

namespace JsonKw {
    //PlayerType
    const QString PlayerType_AI = "AI";
    const QString PlayerType_LocalHuman = "LocalHuman";
    const QString PlayerType_Network = "Network";
    const QString PlayerType_None = "None";
}


const boost::bimap<PlayerType, QString> playerTypeMap = boost::assign::list_of<boost::bimap<PlayerType, QString>::relation>
        (PlayerType::AI, JsonKw::PlayerType_AI)
        (PlayerType::LocalHuman, JsonKw::PlayerType_LocalHuman)
        (PlayerType::Network, JsonKw::PlayerType_Network)
        (PlayerType::None, JsonKw::PlayerType_None);
